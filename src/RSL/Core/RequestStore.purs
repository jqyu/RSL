module RSL.Core.RequestStore
  ( RequestStore(..)
  , BlockedFetches(..)

  , empty
  , add
  , contents
  ) where

import Prelude

import Data.List ( List(..), (:) )
import Data.List as List

import Data.Maybe ( Maybe(..) )

import Data.StrMap ( StrMap(..) )
import Data.StrMap as StrMap

import Unsafe.Coerce ( unsafeCoerce )

import RSL.Core.Types
  ( BlockedFetch
  , class DataSource
  , class Request
  )

import RSL.Utils
  ( typeName )


type BlockedFetches r = List (forall a. BlockedFetch r a)

newtype RequestStore = RequestStore (StrMap forall r. (BlockedFetches r))

empty :: RequestStore
empty = RequestStore $ StrMap.empty

add :: forall r a. (DataSource r, Request r a)
    => BlockedFetch r a
    -> RequestStore
    -> RequestStore
add bf (RequestStore m) =
  RequestStore $ StrMap.alter (unsafeCoerce insertInto) (typeName bf) m
  where insertInto :: Maybe (BlockedFetches r) -> Maybe (BlockedFetches r)
        insertInto Nothing    = return $ unsafeCoerce $ List.singleton bf
        insertInto (Just bfs) = return $ (unsafeCoerce bf : bfs)

contents :: RequestStore -> List forall r. BlockedFetches r
contents (RequestStore m) = StrMap.values m
