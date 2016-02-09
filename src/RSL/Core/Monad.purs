module RSL.Core.Monad where

import Prelude
  ( class Applicative
  , return
  , class Apply
  , class Bind
  , bind
  , class Functor
  , class Show
  , show

  , Unit
  , unit

  , ($)
  , (+)
  , (++)
  , (<$>)
  , (<*>)
  , (>>=)
  , (<<<)
  )

import Control.Bind ( (>=>) )
import Control.Monad.Aff ( Aff )
import Control.Monad.Aff.Console ( log )
import Control.Monad.Eff ( Eff )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Exception ( EXCEPTION, Error, error )
import Control.Monad.Eff.Ref ( REF, Ref, newRef, readRef, modifyRef, writeRef )
import Control.Monad.Except ( throwError )

import Data.Either ( Either(..) )
import Data.Exists ( Exists, mkExists, runExists )
import Data.List ( List(..), length )
import Data.Maybe ( Maybe(..) )
import Data.Traversable ( sequence )

import RSL.Core.Types
  ( Flags
  , defaultFlags

  , Stats(..)
  , emptyStats

  )

import RSL.Core.DataCache ( DataCache )
import RSL.Core.DataCache as DataCache

import RSL.Core.Fetcher
  ( BlockedFetch(..)
  , class Request
  , PerformFetch(..)
  )

import RSL.Core.RequestStore ( RequestStore )
import RSL.Core.RequestStore
  ( empty
  , add
  , apply
  ) as RequestStore

import RSL.Core.ResultVar ( ResultVar, ResultVal(..) )
import RSL.Core.ResultVar
  ( empty
  , read
  ) as ResultVar


--------------------------------------------
-- | Monad Environment

-- The environment contains a bucket of mutable state
-- threaded through a request computation

type Env =
  { cacheRef :: Ref DataCache
  , flags    :: Flags
  , statsRef :: Ref Stats
  }

emptyEnv :: forall eff. Eff ( ref :: REF | eff) Env
emptyEnv = do
  let flags = defaultFlags
  cref <- newRef DataCache.empty
  sref <- newRef emptyStats
  return
    { cacheRef : cref
    , flags    : flags
    , statsRef : sref
    }


--------------------------------------------
-- | The RSL Monad

newtype RSL a
  = RSL forall eff.
    ( Env
      -> Ref RequestStore
      -> Aff ( console :: CONSOLE, ref :: REF | eff ) (Result a)
    )

unRSL :: forall a eff. RSL a
      -> Env
      -> Ref RequestStore
      -> Aff ( console :: CONSOLE, ref :: REF | eff ) (Result a)
         
unRSL (RSL m) = m


data Result a
  = Done a
  | Throw Error
  | Blocked (Cont a)

data CBE a b = CBE (Cont b) (b -> RSL a)
data CAE a b = CAE (Cont (b -> a)) (Cont b)
data CME a b = CME (b -> a) (Cont b)

data Cont a
  = Cont (RSL a)
  | CB (Exists (CBE a))
  | CA (Exists (CAE a))
  | CM (Exists (CME a))

-- Syntactic sugar for continuation constructors
-- , due to the nature of existential types
-- , we cannot use the same tricks for pattern matching

bindCont :: forall a b. Cont b -> (b -> RSL a) -> Cont a
bindCont m k = CB (mkExists (CBE m k))
infixl 1 bindCont as :>>=

applyCont :: forall a b. Cont (b -> a) -> Cont b -> Cont a
applyCont f x = CA (mkExists (CAE f x))
infixl 4 applyCont as :<*>

mapCont :: forall a b. (b -> a) -> Cont b -> Cont a
mapCont f x = CM (mkExists (CME f x))
infixl 4 mapCont as :<$>

-- This computes a blocked continuation, and prunes the tree nicely
-- See the Haxl source for a cleaner implementation
toRSL :: forall a. Cont a -> RSL a
toRSL (Cont rsl) = rsl               -- Unbox
toRSL (CB cont) = runExists to cont  -- Apply :>>=
  where comp :: forall b c. (b -> RSL a) -> CBE b c -> RSL a
        comp k2 (CBE m k1) = toRSL (m :>>= (k1 >=> k2))
        to   :: forall b. CBE a b -> RSL a
        to (CBE (CB c') k) = runExists (comp k) c'
        to (CBE c       k) = toRSL c >>= k
toRSL (CA cont) = runExists to cont  -- Apply :<*>
  where reassoc :: forall b c d. CME (b -> a) c -> CME b d -> RSL a
        reassoc (CME f i) (CME g j) = toRSL (h :<$> i :<*> j)
          where h :: c -> d -> a
                h x y = f x $ g y
        to      :: forall b. CAE a b -> RSL a
        to (CAE (CM c') (CM c'')) = runExists (runExists reassoc c') c''
        to (CAE f       x       ) = toRSL f <*> toRSL x
toRSL (CM cont) = runExists to cont
  where comp :: forall b c. (b -> a) -> CME b c -> RSL a
        comp f (CME g x) = toRSL ((f <<< g) :<$> x)
        to :: forall b. CME a b -> RSL a
        to (CME f (CM c')) = runExists (comp f) c'
        to (CME f x) = f <$> toRSL x

--------------------------------------------
-- | RSL Monad Instances

instance showResult :: (Show a) => Show (Result a) where
  show (Done a) = "Done(" ++ show a ++ ")"
  show (Throw e) = "Throw(" ++ show e ++ ")"
  show (Blocked _) = "Blocked"

-- <$>
instance mapRSL :: Functor RSL where
  map f (RSL m) = RSL \env ref -> do
    r <- m env ref
    case r of
         Done a     -> return $ Done (f a)
         Throw e    -> return $ Throw e
         Blocked a' -> return $ Blocked (f :<$> a')

-- <*>
instance applyRSL :: Apply RSL where
  apply (RSL f) (RSL a) = RSL \env ref -> do
    r <- f env ref
    case r of
         Done f' -> do               -- left is done, explore right
           ra <- a env ref
           case ra of
                Done a'    -> return $ Done (f' a')
                Throw e    -> return $ Throw e 
                Blocked a' -> return $ Blocked (f' :<$> a')
         Throw e -> return (Throw e) -- error
         Blocked f' -> do            -- left is blocked, explore right
           ra <- a env ref
           case ra of
                Done a'    -> return $ Blocked (($ a') :<$> f')
                Throw e    -> return $ Blocked (f' :<*> Cont (throwRSL e))
                Blocked a' -> return $ Blocked (f' :<*> a')

-- return
instance applicativeRSL :: Applicative RSL where
  pure a = RSL \env ref -> return $ Done a

-- >>=
instance bindRSL :: Bind RSL where
  bind (RSL m) k = RSL \env ref -> do
    e <- m env ref
    case e of
         Done a     -> unRSL (k a) env ref
         Throw err  -> return $ Throw err
         Blocked k' -> return $ Blocked (k' :>>= k)

throwRSL :: forall a. Error -> RSL a
throwRSL e = RSL \env ref -> return (Throw e)


--------------------------------------------
-- | RSL Computation Executor

runRSL :: forall a eff. Env -> RSL a
       -> Aff ( console :: CONSOLE, ref :: REF | eff ) a
runRSL env h = do
  -- | Computes a layer of execution with mutation
  let go :: Int -> Env -> Cont a
         -> Aff ( console :: CONSOLE, ref :: REF | eff ) a
      go n env' c = do
        log "START computation"
        -- create an empty request store for current layer of computation
        ref <- liftEff $ newRef RequestStore.empty
        -- computation is performed
        -- SIDE EFFECT: populates request store
        e <- unRSL (toRSL c) env' ref
        log "STOP computation"
        case e of
             Done a     -> return a       -- nothing blocked, move on
             Throw err  -> throwError err -- error thrown
             Blocked c' -> do             -- blocked, eval another round
               bs <- liftEff $ readRef ref
               liftEff $ writeRef ref RequestStore.empty
               log "START performFetches"
               n' <- performFetches n env' bs
               log "STOP performFetches"
               go n' env' c'
  log "START runRSL"
  r <- go 0 env (Cont h)
  log "STOP runRSL"
  return r

--------------------------------------------
-- | Data fetching and caching

-- | Possible responses when checking the cache
data CacheResult a
  = Uncached (ResultVar a)         -- request yet unseen
  | CachedNotFetched (ResultVar a) -- seen and waiting
  | Cached (Either Error a)        -- seen, and waiting

-- checks environment cache for current request
cached :: forall r a eff. (Request r a) => Env -> r a
       -> Aff ( console:: CONSOLE, ref :: REF | eff ) (CacheResult a)
cached env req = do
  cache <- liftEff $ readRef env.cacheRef
  let do_fetch :: Aff ( console :: CONSOLE, ref :: REF | eff ) (CacheResult a)
      do_fetch = do
        log $ "Fetching: " ++ show req
        rvar <- liftEff ResultVar.empty
        liftEff $ writeRef env.cacheRef $ DataCache.insert req rvar cache
        return (Uncached rvar)
  case DataCache.lookup req cache of
       Nothing   -> do_fetch
       Just rvar -> do
         mb <- liftEff $ ResultVar.read rvar
         case mb of
              ResultBlocked -> do
                log $ "Cached, not fetched yet: " ++ show req
                return (CachedNotFetched rvar)
              ResultThrow e -> do
                log $ "Cached error: " ++ show req
                return (Cached (Left e))
              ResultDone a -> do
                log $ "Cached result: " ++ show req
                return (Cached (Right a))

-- Creates an RSL monad reflecting a data request
dataFetch :: forall r a. (Request r a) => r a -> RSL a
dataFetch req = RSL \env ref -> do
  res <- cached env req
  case res of
       -- Uncached, add request to store and return blocked value
       Uncached rvar -> do
         liftEff $ modifyRef ref $ RequestStore.add
           (BlockedFetch { req: req, rvar: rvar })
         return $ Blocked (Cont (continueFetch req rvar))
       -- Cached but not fetched, return blocked value
       CachedNotFetched rvar ->
         return $ Blocked (Cont (continueFetch req rvar))
       -- Evalulated error, throw it
       Cached (Left ex) -> return $ Throw ex
       -- Evaluated result, return it unblocked
       Cached (Right a) -> return $ Done a

-- Bypasses the caching method to queue a request
-- This is mostly used for writes, so you don't cache a repeated query
uncachedRequest :: forall r a. (Request r a) => r a -> RSL a
uncachedRequest req = RSL \env ref -> do
  rvar <- liftEff ResultVar.empty
  liftEff $ modifyRef ref $ RequestStore.add
    (BlockedFetch { req: req, rvar: rvar })
  return $ Blocked (Cont (continueFetch req rvar))

-- Creates a continuation reflecting a blocked value to be retrieved in current fetch
continueFetch :: forall a r. (Request r a)
              => r a -> ResultVar a -> RSL a
continueFetch req rvar = RSL \env ref -> do
  m <- liftEff $ ResultVar.read rvar
  return $ case m of
       ResultDone a -> Done a
       ResultThrow e -> Throw e
       ResultBlocked -> Throw (error "woops, not fetched")

performFetches :: forall eff.
                  Int
               -> Env
               -> RequestStore
               -> Aff ( ref :: REF, console :: CONSOLE | eff ) Int
performFetches n env reqs = do
  let f = env.flags
      fetches = RequestStore.apply f reqs
      n' = n + length fetches
  log "Will collect stats here later"
  log $ "START fetches #" ++ show n'
  executeFetches fetches
  log $ "DONE fetches #" ++ show n'
  return n'

executeFetches :: forall eff.
                  List PerformFetch
               -> Aff ( ref :: REF, console :: CONSOLE | eff ) Unit

executeFetches fe = do
    -- TODO: fork/par this properly
    sequence asyncFetches
    sequence syncFetches
    return unit

  where asyncFetches :: forall eff'.
                        List ( Aff ( ref :: REF, console :: CONSOLE | eff' ) Unit )
        asyncFetches = do
          fetch <- fe
          case fetch of
               AsyncFetch a -> return a
               _            -> Nil

        syncFetches :: forall eff'.
                       List ( Aff ( ref :: REF, console :: CONSOLE | eff' ) Unit )
        syncFetches = do
          fetch <- fe
          case fetch of
               SyncFetch a -> return a
               _           -> Nil
