{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
    (join, forever)
import Data.List
import Glyph
import Model
    (Cell (..), Player (..))

data Action
  = Up Int
  | Down Int
  | Left Int
  | Right Int
  | Attack


type Board = [[Cell]]

data GameState
  = GameState
      { board   :: Board
      , players :: (Player, Player)
      }

newtype Game
  = Game { update :: Action -> GameState -> IO (String, GameState) }

putPlayer :: GameState -> (Int, Int) -> Player -> GameState
putPlayer GameState { board, players } (i, j) player =
    GameState updatedBoard players
  where
    updatedBoard = map f (withIndex board)
    f (i_, row) =
      map (g i_) (withIndex row)

    g _ (_, Water) = Water
    g i_ (j_, Float)
      | j_ == j && i_ == i = Occupied player
      | otherwise = Float
    g _ (_, Occupied p)
        | p == player = Float
        | otherwise = Occupied p
    withIndex xs = zip (findIndices (const True) xs) xs

getPlayer :: GameState -> Player
getPlayer gameState = undefined

rotatePlayer :: GameState -> (Player, GameState)
rotatePlayer gameState = undefined

render :: GameState -> String
render GameState { board } =
  show $ table board


main :: IO ()
main =
  let
    board = [ [Water, Water, Water, Water, Water]
            , [Occupied Ritter, Float, Float, Float, Float]
            , [Float, Float, Float, Float, Float]
            , [Float, Float, Float, Float, Float]
            , [Float, Float, Float, Float, Float]
            , [Float, Float, Float, Float, Occupied Wikinger]
            , [Water, Water, Water, Water, Water]
            ]
    gameState = GameState board (Ritter, Wikinger)
  in forever $ do
    putStrLn $ render gameState

    _ <- getLine
    putStrLn . render $ putPlayer gameState (2, 2) Ritter
    getLine
