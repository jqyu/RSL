module Services.Echo where

import Prelude

import RSL.Core as RSL
import RSL.Utils

import Control.Monad.Aff
import Control.Monad.Aff.Console ( log )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Ref

import Data.Exists
import Data.List
import Data.Traversable

data EchoRequest a
  = Echo a String
  | DoubleEcho a String
  | HelloWorld a

dispatch :: RSL.Flags
         -> List (Exists (RSL.BlockedFetch EchoRequest))
         -> RSL.PerformFetch
dispatch _ reqs = RSL.AsyncFetch do
    sequence fetches
    return unit

  where fetch :: forall a e. RSL.BlockedFetch EchoRequest a -> Aff ( console :: CONSOLE, ref :: REF | e ) Unit
        fetch (RSL.BlockedFetch { req: (Echo a s), rvar: rvar }) = do
          liftEff $ RSL.putSuccess rvar a
          log s
        fetch (RSL.BlockedFetch { req: (DoubleEcho a s), rvar: rvar }) = do
          liftEff $ RSL.putSuccess rvar a
          log $ s ++ s
        fetch (RSL.BlockedFetch { req: (HelloWorld a), rvar: rvar }) = do
          liftEff $ RSL.putSuccess rvar a
          log $ "Hello World"


        fetches :: forall e. List ( Aff ( console :: CONSOLE, ref :: REF | e ) Unit )
        fetches = map (runExists fetch) reqs

instance showRadredisRequest :: Show (EchoRequest a) where
  show (Echo _ s) = s
  show (DoubleEcho _ s) = s ++ s
  show (HelloWorld _) = "hello world"

instance hashRadredisRequest :: Hashable (EchoRequest a) where
  hash (Echo _ s)       = "radredis:echo:" ++ s
  hash (DoubleEcho _ s) = "radredis:decho:" ++ s
  hash (HelloWorld _)   = "radredis:hw:"

instance requestRadredis :: RSL.Request EchoRequest a where
  fetcher r = RSL.new (RSL.key r) dispatch
  key _     = "radredis"
