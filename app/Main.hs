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
  deriving (Eq)


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
      , stdGen  :: StdGen
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

movePlayerBy :: (Int, Int) -> PlayerInfo ->  PlayerInfo
movePlayerBy (di, dj) playerInfo =
  let (i, j) = location playerInfo
  in playerInfo { location = (i + di, j + dj) }

movePlayerRight :: PlayerInfo -> PlayerInfo
movePlayerRight = movePlayerBy (0, 1)

movePlayerLeft :: PlayerInfo -> PlayerInfo
movePlayerLeft = movePlayerBy (0, -1)

movePlayerUp :: PlayerInfo -> PlayerInfo
movePlayerUp = movePlayerBy (-1, 0)

movePlayerDown :: PlayerInfo -> PlayerInfo
movePlayerDown = movePlayerBy (1, 0)

distance :: PlayerInfo -> PlayerInfo -> Int
distance PlayerInfo { location = (i1, j1) } PlayerInfo { location = (i2, j2) } =
  abs (i1 - i2) + abs (j1 - j2)

defaultBoard = [ [Water, Water, Water, Water, Water]
               , [Occupied Ritter, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Float]
               , [Float, Float, Float, Float, Occupied Wikinger]
               , [Water, Water, Water, Water, Water]
               ]

type Game a = State GameState a

rollDice :: Game Int
rollDice = do
  g <- gets stdGen
  let
    (dice, nextG) = roll g
  grantPlayerEnergy dice
  modify (\gameState -> gameState { stdGen = nextG })

  return dice

movePlayer :: (PlayerInfo -> PlayerInfo) -> Game ()
movePlayer fn =
  modify (\gameState ->
    let
      GameState { board, players } = gameState
      playerInfo = fst players
      
      PlayerInfo { energy } = playerInfo
      updatedPlayer = fn playerInfo
      delta = distance updatedPlayer playerInfo
      updatedPlayers = fstUpdate players ( updatedPlayer { energy = energy - delta })

      updatedBoard = updateBoard board updatedPlayer
    in gameState { board = updatedBoard
                 , players = updatedPlayers
                 }
    )

updateBoard :: [[Cell]] -> PlayerInfo -> [[Cell]]
updateBoard board PlayerInfo { player, location = (i, j) } = map f (withIndex board)
  where
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


activePlayerInfo :: GameState -> PlayerInfo
activePlayerInfo = fst . players

getActivePlayer :: Game PlayerInfo
getActivePlayer = gets activePlayerInfo

rotatePlayer :: Game ()
rotatePlayer = do
  swappedPlayers <- swap <$> gets players
  modify (\gameState -> gameState { players = swappedPlayers })

applyAction :: Action -> Game GameState
applyAction action
  | action `elem` [Up, Down, Left, Right] = do
    player <- getActivePlayer
    let
      moveFn = case action of
        Up -> movePlayerUp
        Down -> movePlayerDown
        Left -> movePlayerLeft
        Right -> movePlayerRight
        _ -> const player

    movePlayer moveFn
    get
  | action == Attack = do
    modify (\gameState ->
      let
        (attacker, opponent) = players gameState
        (i, j) = location opponent
        updatedGameState
          | attacker `leftOf` opponent =
            if canMoveRight opponent then
              gameState { players = ( movePlayerRight attacker
                                    , movePlayerRight opponent
                                    )
                        }
            else
              gameState
          | attacker `rightOf` opponent =
            if canMoveLeft opponent then
              gameState { players = ( movePlayerLeft attacker
                                    , movePlayerLeft opponent
                                    )
                        }
            else
              gameState
          | attacker `sixOclockOf` opponent =
            gameState { players = ( movePlayerUp attacker
                                  , movePlayerUp opponent
                                  )
                      }
          | attacker `twelveOclockOf` opponent =
            gameState { players = ( movePlayerDown attacker
                                  , movePlayerDown opponent
                                  )
                      }
          | otherwise = gameState

        updatedAttacker = activePlayerInfo updatedGameState
        updatedOpponent = snd $ players updatedGameState
        updatedBoard = updateBoard (updateBoard (board updatedGameState) updatedOpponent) updatedAttacker
      in updatedGameState { board = updatedBoard }
      )
    get
  | otherwise = get

leftOf :: PlayerInfo -> PlayerInfo -> Bool
leftOf PlayerInfo { location = (i1, j1) } PlayerInfo { location = (i2, j2) } =
  i1 == i2 && j1 == j2 - 1

rightOf :: PlayerInfo -> PlayerInfo -> Bool
rightOf PlayerInfo { location = (i1, j1) } PlayerInfo { location = (i2, j2) } =
  i1 == i2 && j1 == j2 + 1

sixOclockOf :: PlayerInfo -> PlayerInfo -> Bool
sixOclockOf PlayerInfo { location = (i1, j1) } PlayerInfo { location = (i2, j2) } =
  i1 == i2 + 1 && j1 == j2

twelveOclockOf :: PlayerInfo -> PlayerInfo -> Bool
twelveOclockOf PlayerInfo { location = (i1, j1) } PlayerInfo { location = (i2, j2) } =
  i1 == i2 - 1 && j1 == j2

canMoveRight :: PlayerInfo -> Bool
canMoveRight PlayerInfo { location = (_, j) } = j < 4

canMoveLeft :: PlayerInfo -> Bool
canMoveLeft PlayerInfo { location = (_, j) } = j > 0

intoWater :: PlayerInfo -> Bool
intoWater PlayerInfo { location = (i, _) } = i == 0 || i == 6

grantPlayerEnergy :: Int -> Game ()
grantPlayerEnergy energy =
  modify (\gameState ->
  let
    player = activePlayerInfo gameState
    GameState { players } = gameState
    updatedPlayers = fstUpdate players $ player { energy = energy }
  in gameState { players = updatedPlayers }
  )


main :: IO ()
main = do
  g <- newStdGen
  go (initialGameState g)
  where
    initialGameState = GameState defaultBoard
        ( PlayerInfo Ritter (1, 0) 0
        , PlayerInfo Wikinger (5, 4) 0
        )

    go :: GameState -> IO ()
    go gameState = do
      putStrLn $ render gameState

      let
        shouldRollDice = 0 == (energy . activePlayerInfo) gameState
        (dice, m1) = if shouldRollDice
          then runState rollDice gameState
          else (0, gameState)
      when shouldRollDice $
        putStrLn $ "Dice rolled " <> show dice

      putStr "Command (a,s,w,d,c): "
      hFlush stdout
      cmd <- getLine
      let
        maybeActions = (\case
          'd' -> Just Right
          'a' -> Just Left
          'w' -> Just Up
          's' -> Just Down
          'c' -> Just Attack
          _ -> Nothing) <$> cmd
        actions = sequence maybeActions
        m3 = case actions of
          Just actions_ ->
            let
              applyActionMonad = traverse applyAction actions_
              m2 = execState applyActionMonad m1
            in if (energy . fst . players) m2 == 0
              then execState rotatePlayer m2
              else m2
          Nothing -> m1

      go m3
