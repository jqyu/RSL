module RSL.Core.Fetcher
  where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Exception ( Error )

import Data.Exists ( Exists, mkExists, runExists )
import Data.List ( List(..) )

import RSL.Core.Types
  ( Flags
  )

import RSL.Core.ResultVar
  ( ResultVar(..)
  )

import RSL.Utils
  ( class Hashable
  )


--------------------------------
-- | Data Fetcher

class ( Show (r a)
      , Hashable (r a)
      ) <= Request r a where
  fetcher :: r a -> Fetcher r
  key     :: r a -> String

newtype BlockedFetch r a = BlockedFetch
  { req  :: r a
  , rvar :: ResultVar a
  }

data PerformFetch
  = SyncFetch  (   forall e. Aff ( console :: CONSOLE | e ) Unit  )
  | AsyncFetch (  (forall e. Aff ( console :: CONSOLE | e ) Unit)
               -> (forall e. Aff ( console :: CONSOLE | e ) Unit)
               )

type Dispatch r = Flags -> BlockedFetches r -> PerformFetch

type BlockedFetches r = List (Exists (BlockedFetch r))

newtype Fetcher r = Fetcher
  { key      :: String
  , dispatch :: Dispatch r
  , queue    :: BlockedFetches r
  }

new :: forall r. String -> Dispatch r -> Fetcher r
new key dispatch = Fetcher
  { key: key
  , dispatch: dispatch
  , queue: Nil
  }

insert :: forall r a. (Request r a) => BlockedFetch r a -> Fetcher r -> Fetcher r
insert bf (Fetcher { key: k, dispatch: d, queue: q }) =
  Fetcher
    { key: k
    , dispatch: d
    , queue: Cons (mkExists bf) q
    }

bfKey :: forall r a. (Request r a) => BlockedFetch r a -> String
bfKey bf@(BlockedFetch { req: req }) = key req

fromBF :: forall r a. (Request r a) => BlockedFetch r a -> Fetcher r
fromBF bf@(BlockedFetch { req: req }) =
  insert bf (fetcher req)
