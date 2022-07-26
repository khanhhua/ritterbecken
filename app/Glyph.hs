module Glyph where

import Data.Data
    (Typeable)


newtype Glyph' tag
  = Glyph { unwrap :: [String] }
  deriving (Typeable)


retagGlyph :: Glyph' a -> Glyph' b
retagGlyph = Glyph . unwrap

data Series
  = Empty
  | Value (Glyph' Series)
  deriving (Typeable)

infixr 6 ><
(><) :: Glyph' a -> Glyph' a -> Glyph' a
Glyph a >< Glyph b = Glyph $ zipWith (<>) a b

instance Semigroup Series where
  Empty   <> a = a
  Value a <> b = Value $ a ><
    case b of
      Empty   -> Glyph mempty
      Value x -> x

instance Monoid Series where
  mempty = Empty
  mappend = (<>)


class ToGlyph tag where
  toGlyph :: tag -> Glyph' tag
