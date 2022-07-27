{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Monad
    (forever, join)
import Control.Monad.State.Lazy
import Data.List
import Glyph
import Model
    (Cell (..), Player (..))
import Prelude hiding (Left, Right)
import System.IO (hFlush, stdout)

data Action
  = Up
  | Down
  | Left
  | Right
  | Attack


type Board = [[Cell]]
type Location = (Int, Int)

data PlayerInfo = PlayerInfo
  { player :: Player
  , location :: Location
  }

data GameState
  = GameState
      { board   :: Board
      , players :: (PlayerInfo, PlayerInfo)
      }

render :: GameState -> String
render GameState { board } =
  show $ table board

swap :: (a, a) -> (a, a)
swap (p1, p2) = (p2, p1)

fstUpdate :: (a, a) -> a -> (a, a)
fstUpdate (p1, p2) newP = (newP, p2)

defaultBoard = [ [Water, Water, Water, Water, Water]
               , [Occupied Ritter, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Occupied Wikinger]
               , [Water, Water, Water, Water, Water]
               ]

type Game a = State GameState a

movePlayer :: (Int, Int) -> Player -> Game ()
movePlayer (i, j) player = do
  GameState { board, players } <- get
  let
    updatedBoard = updateBoard board
    updatedPlayers = fstUpdate players (PlayerInfo player (i, j))
  put $ GameState updatedBoard updatedPlayers

  where
    updateBoard board = map f (withIndex board)
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

getActivePlayer :: Game PlayerInfo
getActivePlayer = gets (fst . players)

rotatePlayer :: Game ()
rotatePlayer = do
  swappedPlayers <- swap <$> gets players
  modify (\GameState { board } -> GameState board swappedPlayers)

applyAction :: Action -> Game GameState
applyAction action = do
  PlayerInfo { player, location } <- getActivePlayer
  let
    (i, j) = location
    updatedLocation = case action of
      Up -> (i - 1, j)
      Down -> (i + 1, j)
      Left -> (i, j - 1)
      Right -> (i, j + 1)
      _ -> location
      
  movePlayer updatedLocation Ritter
  get

main :: IO ()
main =
  go initialGameState
  where
    initialGameState = GameState defaultBoard
        ( PlayerInfo Ritter (1, 0)
        , PlayerInfo Wikinger (5, 4)
        )

    go :: GameState -> IO ()
    go gameState = do
      putStrLn $ render gameState
      putStr "Command (A,S,W,D): "
      hFlush stdout
      cmd <- getLine
      let
        actions = (\case
          'D' -> Right
          'A' -> Left
          'W' -> Up
          'S' -> Down
          _ -> undefined) <$> cmd
        m = traverse applyAction actions
        updatedGameState = execState m gameState

      go updatedGameState
