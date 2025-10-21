-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Set
import Data.Array
import System.Random


type Cell     = (Int, Int)      -- Grid coordinates
type WorldPos = (Float, Float)  -- World (screen) coordinates

data Tile = Wall | Empty | Gate --Tiles options
  deriving (Eq, Show)

type Maze = Array Cell Tile     --Maze as array of tiles


data Pellets = Pellets          --Pellets locations
  { 
    dots      :: Set Cell,
    powerDots :: Set Cell
  } 
  deriving (Eq, Show)


--Movement primitives
data Direction = U | D | L | R | None 
  deriving (Eq, Show)

newtype Speed = Speed Float 
  deriving (Eq, Show)

--actors
data Pacman = Pacman
  { 
    pPos   :: WorldPos,
    pDir   :: Direction,
    pSpeed :: Speed,
    pNext  :: Direction
  } 
  deriving (Eq, Show)

data GhostMode = Chase | Frightened 
  deriving (Eq, Show)

data Ghost = Ghost
  { 
    gPos   :: WorldPos,
    gDir   :: Direction,
    gMode  :: GhostMode,
    gSpeed :: Speed
  } 
  deriving (Eq, Show)

--UIstate
data UIState = UIState
  { 
    highScores :: [(String, Int)],
    message    :: Maybe String
  } 
  deriving (Eq, Show)


--The World state
data GameState = GameState
  { 
  maze        :: Maze,
  pellets     :: Pellets,
  pacman      :: Pacman,
  ghosts      :: [Ghost],
  score       :: Int,
  lives       :: Int,
  level       :: Int,
  paused      :: Bool,
  random      :: StdGen, -- Standard random number generator
  ui          :: UIState,
  elapsedTime :: Float
  }
 deriving (Eq, Show)


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

initialState :: GameState
initialState = GameState 
  (array ((1, 2), (1, 3)) [((1, 2), Wall)]) 
  (Pellets (Data.Set.fromList [(1,1),(1,2)]) Data.Set.empty)
  (Pacman (1.2, 1.2) U (Speed 3.2) D)
  [Ghost (1.2, 1.2) U Chase (Speed 3.2), Ghost (1.2, 1.2) U Chase (Speed 3.2)]
  10
  12
  54
  False
  (mkStdGen 6)
  (UIState [("wqe", 23), ("2vwe", 32)] (Just "qqwe"))
  0