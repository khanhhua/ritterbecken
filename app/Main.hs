{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
    (join)
import Data.List
import Glyph
import Model
    (Cell (..), Player (..))

data Action
  = Forward Int
  | Left Int
  | Right Int
  | Attack


newtype Board
  = Board [Cell]

instance Show Board where
    show (Board (c00:c01:c02:c03:c04:
                c10:c11:c12:c13:c14:
                c20:c21:c22:c23:c24:
                c30:c31:c32:c33:c34:
                c40:c41:c42:c43:c44:
                c50:c51:c52:c53:c54:
                c60:c61:c62:c63:c64:
                c70:c71:c72:c73:c74:_)
        ) = showRow [c00, c01, c02, c03, c04] <> "\n" <>
            showRow [c10, c11, c12, c13, c14] <> "\n" <>
            showRow [c20, c21, c22, c23, c24] <> "\n" <>
            showRow [c30, c31, c32, c33, c34] <> "\n" <>
            showRow [c40, c41, c42, c43, c44] <> "\n" <>
            showRow [c50, c51, c52, c53, c54] <> "\n" <>
            showRow [c60, c61, c62, c63, c64] <> "\n" <>
            showRow [c70, c71, c72, c73, c74]
    show _ = error "Invalid board"


showRow :: Show a => [a] -> String
showRow =
    concatMap show

data GameState
  = GameState
      { board   :: Board
      , players :: (Player, Player)
      }

newtype Game
  = Game { update :: Action -> GameState -> IO (String, GameState) }

putPlayer :: GameState -> (Int, Int) -> Player -> GameState
putPlayer GameState { board, players } (i, j) player =
    GameState (Board updatedBoard) players
  where
    updatedBoard = map f (withIndex board)
    f (index, Water) = Water
    f (index, Float)
        | index == i * 5 + j = Occupied player
        | otherwise = Float
    f (index, Occupied p)
        | p == player = Float
        | otherwise = Occupied p
    withIndex (Board xs) = zip (findIndices (const True) xs) xs

getPlayer :: GameState -> Player
getPlayer gameState = undefined

rotatePlayer :: GameState -> (Player, GameState)
rotatePlayer gameState = undefined

render :: GameState -> String
render GameState { board, players } =
    show board


main :: IO ()
main = undefined
