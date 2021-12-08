module Main where

import GameLogic
import View
import Controller

main :: IO ()
main =
  do
    runActivity $
      resettable $
      withUndo $
      withStartScreen $
      Activity (initialState 0) handleEvent (pictureToString . drawState)
