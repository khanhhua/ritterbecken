module Model where

import Glyph
    (Glyph' (Glyph), ToGlyph (..))

data Player = Ritter | Wikinger deriving (Eq)

instance Show Player where
  show Ritter   = "R"
  show Wikinger = "W"

data Cell
  = Water
  | Float
  | Occupied Player

instance ToGlyph Cell where
  toGlyph Water =        Glyph [ "~~~~~"
                               , "~~~~~"
                               , "~~~~~"
                               ]
  toGlyph Float =        Glyph [ ".===."
                               , "|===|"
                               , ".___."
                               ]
  toGlyph (Occupied p) = Glyph [ ".---."
                               , "|_" <> show p <> "_|"
                               , ".___."
                               ]


instance Show Cell where
    show Water        = "~"
    show Float        = "_"
    show (Occupied p) = show p


