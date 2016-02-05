module RSL.Core.Monad where

-- TODO: explicit imports
import Prelude

import Control.Bind
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Control.Monad.Eff
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Except

import Data.Exists
import Data.List

import RSL.Core.Types
  ( Flags(..)
  , defaultFlags

  , Stats(..)
  , emptyStats

  , ResultVar(..)
  )

import RSL.Core.DataCache ( DataCache(..) )
import RSL.Core.DataCache as DataCache

import RSL.Core.RequestStore ( RequestStore(..) )
import RSL.Core.RequestStore as RequestStore

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
      -> Aff ( ref :: REF | eff ) (Result a)
    )

unRSL :: forall a eff. RSL a
      -> Env
      -> Ref RequestStore
      -> Aff ( ref :: REF | eff ) (Result a)
         
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

runRSL :: forall a eff.
          Env
       -> RSL a
       -> Aff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF | eff ) a
runRSL env h = do
  -- | Computes a layer of execution with mutation
  let go :: Int -> Env -> Cont a -> Aff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF | eff ) a
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

dataFetch :: forall r a. r a -> RSL a
dataFetch req = RSL \env ref -> do
  return $ Throw (error "yes")

performFetches :: forall eff.
                  Int
               -> Env
               -> RequestStore
               -> Aff ( ref :: REF | eff ) Int
performFetches n env reqs = do
  let f = env.flags
      jobs = RequestStore.contents reqs
      n' = n + length jobs
  return n'


