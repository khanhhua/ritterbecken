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

infixr 6 ><
(><) :: Glyph' a -> Glyph' a -> Glyph' a
Glyph a >< Glyph b = Glyph $ zipWith (<>) a b

instance ToGlyph a => Semigroup (Glyph' a) where
  (Glyph []) <> a = a
  a <> (Glyph []) = a
  a <> b          = a >< b
instance ToGlyph a => Monoid (Glyph' a) where
  mempty = Glyph []
  mappend = (<>)


data Stride a
  = Empty
  | Stride (Glyph' a)

instance ToGlyph a =>Show (Stride a) where
  show Empty      = ""
  show (Stride a) = show a

instance ToGlyph a => Semigroup (Stride a) where
  Empty <> a = a
  a <> Empty = a
  Stride a <> Stride b = Stride . Glyph $
    let
      (Glyph xs) = a
      (Glyph ys) = b
    in  [ intercalate "\n" xs
        , intercalate "\n" ys
        ]

instance ToGlyph a => Monoid (Stride a) where
  mempty = Empty
  mappend = (<>)


class ToGlyph tag where
  toGlyph :: tag -> Glyph' tag

singleton :: ToGlyph a => a -> Glyph' a
singleton = retagGlyph' . toGlyph

rowBreak :: ToGlyph a => Glyph' a
rowBreak = Glyph $ repeat "\n"

row' :: ToGlyph a => [a] -> Glyph' a
row' (x:xs) = singleton x <> row' xs
row' _      = mempty

row :: ToGlyph a => [a] -> Stride a
row xs
  | null xs   = mempty
  | otherwise = Stride $ row' xs

table :: ToGlyph a => [[a]] -> Stride a
table lines = foldl (<>) mempty rows
  where
    rows = map row lines
