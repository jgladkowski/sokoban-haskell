module View where

import Levels
import GameLogic

-- Picture data type definition

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

-- Basic definitions for Picture data type

blank :: Picture
blank = id

(&) :: Picture -> Picture -> Picture
(&) = (.)

fromPicture :: Picture -> DrawFun
fromPicture = ($(\_ _ -> ' '))

toPicture :: DrawFun -> Picture
toPicture df1 df2 x y =
    if character == ' ' then df2 x y else character
    where character = df1 x y

-- Characters displayed on screen while drawing a maze

player :: Char
player = '@'

drawTile :: Tile -> Char
drawTile Wall    = '#'
drawTile Storage = '.'
drawTile Box     = '$'
drawTile Ground  = ' '
drawTile Blank   = ' '

-- For displaying text on screen

lettering :: String -> Picture
lettering str = toPicture (\x y ->
  let n = 1 + (length str) `div` 2 in
  if y /= 0 || fromInteger x < -n || (fromInteger x + n) >= (length str) then ' '
  else str !! (fromInteger x + n)
  )

startScreen :: Picture
startScreen = lettering "Sokoban"

winningScreen :: Integer -> Picture
winningScreen n = lettering $ "Level complete in " ++ (show n) ++ " moves!"

-- Drawing state on screen

drawState :: State -> Picture
drawState state
  | isWinning state = winningScreen $ movesMade state
  | otherwise =
    toPicture (\x y ->
      if elem (C x y) $ boxesCoords state then drawTile Box else ' ') &
    toPicture (\x y ->
      if C x y == playerPosition state then player else ' ') &
    toPicture (\x y -> drawTile $ tiles $ C x y)
    where (Maze _ tiles) = removeBoxes $ mazes !! currentLevel state

-- Transforming Picture to terminal format

screenHeight :: Integer
screenHeight = 23

screenWidth :: Integer
screenWidth = 80

pictureToString :: Picture -> String
pictureToString picture =
  [if x == maxX + 1 then '\n' else (fromPicture picture) x (-y)
    | y <- [-maxY..maxY], x <- [-maxX..maxX+1]]
  where
    maxX = screenWidth  `div` 2
    maxY = screenHeight `div` 2
