module Glyph where

import Data.Data
    (Typeable)
import Data.List
    (intercalate, intersperse)


newtype Glyph' tag
  = Glyph { unwrap :: [String] }
  deriving (Typeable)

retagGlyph' :: Glyph' a -> Glyph' b
retagGlyph' = Glyph . unwrap

instance ToGlyph a => Show (Glyph' a) where
  show (Glyph xs) = intercalate "\n" xs
-- data Series
--   = Empty
--   | Value (Glyph' Series)
--   deriving (Typeable)

infixr 6 ><
(><) :: Glyph' a -> Glyph' a -> Glyph' a
Glyph a >< Glyph b = Glyph $ zipWith (<>) a b

instance ToGlyph a => Semigroup (Glyph' a) where
  (Glyph []) <> a = a
  a <> (Glyph []) = a
  a <> b      = a >< b
instance ToGlyph a => Monoid (Glyph' a) where
  mempty = Glyph []
  mappend = (<>)

-- instance Semigroup Series where
--   Empty   <> a = a
--   Value a <> b = Value $ a ><
--     case b of
--       Empty   -> Glyph mempty
--       Value x -> x

-- instance Monoid Series where
--   mempty = Empty
--   mappend = (<>)


class ToGlyph tag where
  toGlyph :: tag -> Glyph' tag


row :: ToGlyph a => [a] -> Glyph' a
row (x:xs) = singleton x <> row xs
row _      = mempty

singleton :: ToGlyph a => a -> Glyph' a
singleton x = retagGlyph' $ toGlyph x
