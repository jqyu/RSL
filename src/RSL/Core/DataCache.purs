module RSL.Core.DataCache
  ( DataCache(..)
  , empty
  ) where

import Prelude

import Data.StrMap ( StrMap(..) )
import Data.StrMap as StrMap

import Data.Exists
  ( Exists(..)
  )

import RSL.Core.Types
  ( ResultVar(..)
  )

newtype DataCache = DataCache (StrMap (Exists ResultVar))

empty :: DataCache
empty = DataCache $ StrMap.empty
