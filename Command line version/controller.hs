module Controller where

import System.IO
import Levels
import GameLogic
import View

-- Data types

data Event  = KeyPress String
type Screen = String

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Screen)

-- I/O

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) =
  do
    hSetBuffering stdin NoBuffering
    input <- getContents
    go state0 input
    where
      go state stream =
        do
          putStr "\ESCc"
          putStr (draw state)
          let (key, newStream) = getKey stream
          let newState = handle (KeyPress key) state
          go newState newStream

getKey :: String -> (String, String)
getKey ('\ESC':'[':'A':rest) = ("Up",    rest)
getKey ('\ESC':'[':'B':rest) = ("Down",  rest)
getKey ('\ESC':'[':'C':rest) = ("Right", rest)
getKey ('\ESC':'[':'D':rest) = ("Left",  rest)
getKey ('\ESC':'[':_:rest)   = ("Esc",   rest)
getKey ('\ESC':_:rest)       = ("Esc",   rest)
getKey (c:rest)              = ([c],     rest)

-- Resettable activity

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw) =
  Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start Screen activity

data SSState world = StartScreen | Running world deriving Eq

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) =
  Activity state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)
    draw' StartScreen = pictureToString startScreen
    draw' (Running s) = draw s

-- Undo activity

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state =
  let
    nextLevel =
      if length mazes > currentLevel state + 1 then
        initialState $ currentLevel state + 1
      else
        state
  in
  if isWinning state then
    if key == " " then nextLevel
    else state
  else if key == "n" then nextLevel
  else let
    currentCoord = playerPosition state
    maybeDirection
      | key == "Up"    = Just U
      | key == "Left"  = Just L
      | key == "Down"  = Just D
      | key == "Right" = Just R
      | otherwise      = Nothing
  in case maybeDirection of
    Nothing -> state
    Just direction ->
      let
        nextCoord1@(C x1 y1) = getNewCoord currentCoord direction
        nextCoord2           = getNewCoord nextCoord1   direction
        (Maze _ currentMaze) = addBoxes (boxesCoords state) (removeBoxes $ mazes !! currentLevel state)
        state'               = state { movesMade = 1 + movesMade state }
      in
      if isPassable $ currentMaze nextCoord1 then
        state' { playerPosition = nextCoord1 }
      else if (currentMaze nextCoord1 == Box) && (isPassable $ currentMaze nextCoord2) then
        state' { playerPosition = nextCoord1,
          boxesCoords = map (\coord -> if coord == nextCoord1 then nextCoord2 else coord) (boxesCoords state) }
      else state'
