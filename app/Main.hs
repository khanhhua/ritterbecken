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
import Dice ( roll )
import Prelude hiding (Left, Right)
import System.IO (hFlush, stdout)
import System.Random (StdGen, newStdGen)
import Data.Maybe (isJust, fromJust)


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
  , energy :: Int
  }

instance Show PlayerInfo where 
  show (PlayerInfo player location energy) =
    "Player: " <> show player
      <> " at " <> show location
      <> " energy " <> show energy

data GameState
  = GameState
      { board   :: Board
      , players :: (PlayerInfo, PlayerInfo)
      }

render :: GameState -> String
render GameState { board, players } =
  let
    t = show $ table board
    p = show $ fst players
  in t <> "\n" <> p
  

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
    PlayerInfo _ (i_, j_) energy = fst players
    delta = abs (i_ - i) + abs (j_ - j)
    updatedPlayers = fstUpdate players (PlayerInfo player (i, j) (energy - delta))

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
      
  movePlayer updatedLocation player
  get

grantPlayerEnergy :: Int -> Game ()
grantPlayerEnergy energy = do
  gameState <- get
  activePlayer <- getActivePlayer
  let
    GameState { players } = gameState
    updatedPlayers = fstUpdate players $ activePlayer { energy = energy }
  put gameState { players = updatedPlayers }

main :: IO ()
main = do
  g <- newStdGen
  go initialGameState g
  where
    initialGameState = GameState defaultBoard
        ( PlayerInfo Ritter (1, 0) 0
        , PlayerInfo Wikinger (5, 4) 0
        )

    go :: GameState -> StdGen -> IO ()
    go gameState g = do
      putStrLn $ render gameState
      let
        (m1, dice, nextG) = if (energy . fst . players) gameState == 0
          then
            let
              (dice, nextG) = roll g
            in (execState (grantPlayerEnergy dice) gameState, Just dice, nextG)
          else (gameState, Nothing, g)
      when (isJust dice) $ do
        putStrLn $ "System has rolled " <> show (fromJust dice)

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
        applyActionMonad = traverse applyAction actions
        m2 = execState applyActionMonad m1
        m3 = if (energy . fst . players) m2 == 0
          then execState rotatePlayer m2
          else m2

      go m3 nextG
