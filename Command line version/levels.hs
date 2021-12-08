module Levels where

-- Data types

data Tile  = Wall | Ground | Storage | Box | Blank deriving Eq
data Coord = C Integer Integer                     deriving Eq

data Maze = Maze Coord (Coord -> Tile)

-- The size of levels

levelSize :: Integer
levelSize = 20

-- Levels list

mazes :: [Maze]
mazes = [maze0, maze1, maze2, maze3]

maze0 :: Maze
maze0 = Maze (C (-1) (-2)) maze
  where maze (C x y)
          | abs x > 4  || abs y > 4  = Blank
          | abs x == 4 || abs y == 4 = Wall
          | x ==  2 && y <= 0        = Wall
          | x ==  3 && y <= 0        = Storage
          | x >= -2 && y == 0        = Box
          | otherwise                = Ground

maze1 :: Maze
maze1 = Maze (C (-1) 0) maze
  where maze (C x y)
          | x == 0 && abs y < 2      = Box
          | x == 0 && abs y == 2     = Storage
          | x == 2 && y == 0         = Storage
          | abs x < 2 && y == 0      = Ground
          | abs x < 4 && abs y < 4   = Wall
          | otherwise                = Blank

maze2 :: Maze
maze2 = Maze (C (-2) (-1)) maze
  where maze (C x y)
          | abs x > 7 || abs y > 7        = Blank
          | abs x == 7 || abs y == 7      = Wall
          | x == 0 && y == 0              = Storage
          | x == 0 && y == -1             = Box
          | x == -2 && y == 0             = Storage
          | x == -4 && y == 5             = Box
          | y == 6 && -6 <= x && x <= -4  = Ground
          | y == 6 && 4 <= x && x <= 5    = Ground
          | y == 5 && -6 <= x && x <= 5   = Ground
          | x == 5 && -5 <= y && y <= 6   = Ground
          | x == 6 && -5 <= y && y <= -4  = Ground
          | y == -5 && -5 <= x && x <= 6  = Ground
          | y == -6 && -5 <= x && x <= -4 = Ground
          | x == -5 && -6 <= y && y <= 2  = Ground
          | x == -6 && 1 <= y && y <= 2   = Ground
          | y == 2 && -6 <= x && x <= 2   = Ground
          | y == 3 && 1 <= x && x <= 2    = Ground
          | x == 2 && -2 <= y && y <= 3   = Ground
          | x == 3 && -2 <= y && y <= -1  = Ground
          | y == -2 && -2 <= x && x <= 3  = Ground
          | y == -3 && -2 <= x && x <= -1 = Ground
          | x == -2 && y == -1            = Ground
          | otherwise                     = Wall

maze3 :: Maze
maze3 = Maze (C 3 1) maze
  where maze (C x y)
          | abs x > 4 || abs y > 2          = Blank
          | abs x == 4 || abs y == 2        = Wall
          | x == 3 && y == -1               = Wall
          | x == -3 || (x == -2 && y == 1)  = Storage
          | x == 0 && y /= -1               = Box
          | x == 1 && y /= 1                = Box
          | otherwise                       = Ground
