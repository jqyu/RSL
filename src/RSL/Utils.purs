module RSL.Utils
  ( class Typeable
  , typeName
  , class Hashable
  , hash

  , ExistsK(..)
  , mkExistsK
  , runExistsK
  ) where


-- Exists for Kinds
foreign import data ExistsK :: ((* -> *) -> *) -> *
foreign import mkExistsK :: forall f a. f a -> ExistsK f
foreign import runExistsK :: forall f r. (forall a. f a -> r) -> ExistsK f -> r

class Typeable a where
  typeName :: a -> String

class Hashable a where
  hash :: a -> String
