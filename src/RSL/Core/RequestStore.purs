module RSL.Core.RequestStore
  ( RequestStore(..)

  , empty
  , add
  , apply
  ) where


import Prelude

import Data.List ( List(..), (:) )
import Data.List as List

import Data.Maybe ( Maybe(..) )

import Data.StrMap ( StrMap )
import Data.StrMap as StrMap

import Unsafe.Coerce ( unsafeCoerce )

import RSL.Core.Fetcher
  ( BlockedFetch(..)
  , Fetcher(..)
  , class Request
  , PerformFetch
  )
import RSL.Core.Fetcher
  ( new
  , insert
  , bfKey
  , fromBF
  ) as Fetcher

import RSL.Core.Types ( Flags )
import RSL.Utils
  ( ExistsK
  , mkExistsK
  , runExistsK
  )

newtype RequestStore = RequestStore (StrMap (ExistsK Fetcher))

empty :: RequestStore
empty = RequestStore StrMap.empty

add :: forall r a. (Request r a)
    => BlockedFetch r a
    -> RequestStore
    -> RequestStore
add bf (RequestStore m) = RequestStore $
  StrMap.alter (Just <<< mkExistsK <<< insert) key m

  where key :: String
        key = Fetcher.bfKey bf

        insertInto :: forall b. Fetcher b -> Fetcher r
        insertInto fe = Fetcher.insert bf $ unsafeCoerce fe

        insert :: Maybe (ExistsK Fetcher) -> Fetcher r
        insert Nothing = Fetcher.fromBF bf
        insert (Just rq) = runExistsK insertInto rq

apply :: Flags -> RequestStore -> List PerformFetch
apply f (RequestStore m) = map apply' $ StrMap.values m
  where dispatch' :: forall r. Fetcher r -> PerformFetch
        dispatch' (Fetcher { dispatch: d, queue: q }) = d f q
        apply'    :: ExistsK Fetcher -> PerformFetch
        apply' fe = runExistsK dispatch' fe
