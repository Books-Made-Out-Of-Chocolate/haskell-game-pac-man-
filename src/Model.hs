-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Set as S
import Data.Array
import System.Random
import Text.ParserCombinators.ReadP (char)
import GHC.Real (fromIntegral)

--model of program
data Model = Model
  {
    gState :: GameState,
    inputs :: InputControls
  }


--game data structs
type Cell     = (Int, Int)      -- Grid coordinates
type WorldPos = (Float, Float)  -- World (screen) coordinates

data Tile = Wall | Empty | Gate | Pellet | PowerPellet --Tiles options
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
    pPos    :: WorldPos,
    pStepsX :: Float,
    pStepsY :: Float,
    pCell   :: Cell,
    pReversing :: Bool,
    pDir    :: Direction,
    pSpeed  :: Speed,
    pNext   :: Direction
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
amountSecondsBetweenStep = 0.1

tileSize :: Float
tileSize = 10

initialModel :: IO Model
initialModel = do
  maze <- loadMazeFromFile "src/levels/maze2.txt"
  return $ Model
            (GameState
                  maze
                  (Pellets (S.fromList [(1,1),(1,2)]) S.empty)
                  (makePacman (9,19) U (10 * tileSize) U)
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

makePacman :: Cell -> Direction -> Speed -> Direction -> Pacman
makePacman (x, y) = Pacman ((fromIntegral x * tileSize) + (tileSize / 2), 
                            (fromIntegral y * tileSize) + (tileSize / 2)) 0.0 0.0 (x, y) False


-- maze constamts
-- === Maze-constanten ===
gridMinX, gridMaxX, gridMinY, gridMaxY :: Int
gridMinX = 0; gridMaxX = 27
gridMinY = 0; gridMaxY = 35

gridBounds :: (Cell, Cell)
gridBounds = ((gridMinX, gridMinY), (gridMaxX, gridMaxY))

createMaze :: [[Char]] -> Maze
createMaze rows =
  let height = length rows
      width  = length (head rows)
      cells  = [ (x,y) | y <- [0..height-1], x <- [0..width-1] ]
      tileAt (x,y) =
        case (rows !! y) !! x of
          '#' -> Wall
          '.' -> Pellet
          'o' -> PowerPellet
          '-' -> Empty
          'G' -> Gate
          _   -> Empty
  in Data.Array.array ((0,0), (width-1, height-1)) [ (c, tileAt c) | c <- cells ]

loadMazeFromFile :: FilePath -> IO Maze
loadMazeFromFile filepath = do
  content <- readFile filepath
  let rows = lines content
  return (createMaze rows)



