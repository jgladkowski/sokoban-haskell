module GameLogic where

import Levels

-- Data Types

data Direction = R | U | L | D deriving Eq
data State = S {
  playerPosition :: Coord,
  boxesCoords    :: [Coord],
  currentLevel   :: Int,
  movesMade      :: Integer
} deriving Eq

-- Functions useful for implementing movement

isPassable :: Tile -> Bool
isPassable Ground  = True
isPassable Storage = True
isPassable _       = False

getNewCoord :: Coord -> Direction -> Coord
getNewCoord (C x y) U = C x (y + 1)
getNewCoord (C x y) L = C (x - 1) y
getNewCoord (C x y) D = C x (y - 1)
getNewCoord (C x y) R = C (x + 1) y

-- State related functions

isWinning :: State -> Bool
isWinning state =
  all
    (\coord -> tiles coord == Storage)
    (boxesCoords state)
  where (Maze _ tiles) = mazes !! currentLevel state

initialState :: Int -> State
initialState n =
  S { playerPosition = initial,
      boxesCoords    = initialBoxes maze,
      currentLevel   = n,
      movesMade      = 0 }
  where maze@(Maze initial _) = mazes !! n

-- Maze related functions

removeBoxes :: Maze -> Maze
removeBoxes (Maze initial tiles) = Maze initial $ f . tiles
  where
    f Box = Ground
    f tile = tile

addBoxes :: [Coord] -> Maze -> Maze
addBoxes [] maze = maze
addBoxes ((C x1 y1):t) maze@(Maze initial tiles) = Maze initial $
  \coord@(C x2 y2) -> if x1 == x2 && y1 == y2 then Box else tiles' coord
  where (Maze _ tiles') = addBoxes t maze

initialBoxes :: Maze -> [Coord]
initialBoxes maze@(Maze initial tiles) =
  [C x y | x <- [-levelSize..levelSize],
           y <- [-levelSize..levelSize],
           (tiles $ C x y) == Box]
