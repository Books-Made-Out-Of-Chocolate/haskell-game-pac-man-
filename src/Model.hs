-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Set as S
import Data.Array
import System.Random

--model of program
data Model = Model
  {
    gState :: GameState,
    inputs :: InputControls
  }


--game data structs
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

type Speed = Float

--actors
data Pacman = Pacman
  { 
    pPos   :: WorldPos,
    pCell  :: Cell,
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
    gCell  :: Cell,
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
--end game data structs

--input structs
data PressedControls = PressedControls
  {
    pause :: Bool,
    reset :: Bool,
    enter :: Bool
  }
 deriving (Eq, Show)  

data CharsHighScoreInput = CharsHighScoreInput
  {
    char1 :: Char,
    char2 :: Char,
    char3 :: Char,
    char4 :: Char
  }
 deriving (Eq, Show)  

data InputControls = InputControls 
  {
    keys  :: PressedControls,
    chars :: CharsHighScoreInput
  }
 deriving (Eq, Show)   

--random shit
amountSecondsBetweenStep :: Float
amountSecondsBetweenStep = 1

initialModel :: Model
initialModel = Model 
                (GameState
                      emptyMazeWithBorder
                      (Pellets (S.fromList [(1,1),(1,2),(6,2)]) (S.fromList [(9, 2), (21, 4)]))
                      (Pacman (200, 10) (20, 1) U 2.5 U)
                      [Ghost (1.2, 1.2) (0, 0) U Chase 3.2, Ghost (1.2, 1.2) (0, 0) U Chase 3.2]
                      10
                      12
                      54
                      False
                      (mkStdGen 6)
                      (UIState [("wqe", 23), ("2vwe", 32)] (Just "qqwe"))
                      0
                )
                (InputControls 
                      (PressedControls False False False)
                      (CharsHighScoreInput ' ' ' ' ' ' ' ')
                )

--end game data structs


-- maze constamts
-- === Maze-constanten ===
gridMinX, gridMaxX, gridMinY, gridMaxY :: Int
gridMinX = 0; gridMaxX = 27
gridMinY = 0; gridMaxY = 35

gridBounds :: (Cell, Cell)
gridBounds = ((gridMinX, gridMinY), (gridMaxX, gridMaxY))


emptyMazeWithBorder :: Maze
emptyMazeWithBorder =
  let cells = [ (x,y) | y <- [gridMinY..gridMaxY], x <- [gridMinX..gridMaxX] ]
      isBorder (x,y) =
        x == gridMinX || x == gridMaxX || y == gridMinY || y == gridMaxY

      innerWalls =
        S.fromList ([(x,10) | x <- [3..24]] ++
                    [(x,20) | x <- [3..24]] ++
                    [(8,y)  | y <- [12..18]] ++
                    [(19,y) | y <- [12..18]])
      tileAt c
        | isBorder c           = Wall
        | c `S.member` innerWalls = Wall
        | otherwise            = Empty
  in Data.Array.array gridBounds [ (c, tileAt c) | c <- cells ]
