module Services.Radredis where

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

data RadredisRequest a
  = Echo a String

dispatch :: RSL.Flags
         -> List (Exists (RSL.BlockedFetch RadredisRequest))
         -> RSL.PerformFetch
dispatch _ reqs = RSL.AsyncFetch do
    sequence fetches
    return unit
  where fetch :: forall a e. RSL.BlockedFetch RadredisRequest a -> Aff ( console :: CONSOLE, ref :: REF | e ) Unit
        fetch (RSL.BlockedFetch { req: (Echo a s), rvar: rvar }) = do
          liftEff $ RSL.putSuccess rvar a
          log s
        fetches :: forall e. List ( Aff ( console :: CONSOLE, ref :: REF | e ) Unit )
        fetches = map (runExists fetch) reqs

instance showRadredisRequest :: Show (RadredisRequest a) where
  show (Echo _ s) = s

instance hashRadredisRequest :: Hashable (RadredisRequest a) where
  hash (Echo _ s) = "radredis:echo:" ++ s

instance requestRadredis :: RSL.Request RadredisRequest a where
  fetcher r = RSL.new (RSL.key r) dispatch
  key _     = "radredis"
