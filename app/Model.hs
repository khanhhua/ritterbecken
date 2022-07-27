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
  toGlyph Water =        Glyph [ "+-W-+"
                               , "|~~~|"
                               , "+---+"
                               ]
  toGlyph Float =        Glyph [ "+-F-+"
                               , "|   |"
                               , "+---+"
                               ]
  toGlyph (Occupied p) = Glyph [ "+-P-+"
                               , "|_" <> show p <> "_|"
                               , "+---+"
                               ]


instance Show Cell where
    show Water        = "~"
    show Float        = "_"
    show (Occupied p) = show p


