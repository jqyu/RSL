module RSL.Core.RequestStore
  ( BlockedFetches(..)
  , RequestStore(..)

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

data BlockedFetches =
  BlockedFetches
    (  forall r. (DataSource r)
    => List (forall a. BlockedFetch r a)
    )
newtype RequestStore = RequestStore (StrMap BlockedFetches)

empty :: RequestStore
empty = RequestStore StrMap.empty

add :: forall r a. (DataSource r, Request r a)
    => BlockedFetch r a
    -> RequestStore
    -> RequestStore
add bf (RequestStore m) =
  RequestStore $ StrMap.alter insertInto (typeName bf) m
  where insertInto' :: forall r'. BlockedFetch r a
                    -> List (forall a'. BlockedFetch r' a')
                    -> List (forall a'. BlockedFetch r' a')
        insertInto' bf bfs = unsafeCoerce (bf : unsafeCoerce bfs)

        insertInto :: Maybe BlockedFetches -> Maybe BlockedFetches
        insertInto Nothing =
          return $ BlockedFetches (insertInto' bf Nil)
        insertInto (Just (BlockedFetches bfs)) =
          return $ BlockedFetches (insertInto' bf bfs)

contents :: RequestStore -> List BlockedFetches
contents (RequestStore m) = StrMap.values m
