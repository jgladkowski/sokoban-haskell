import Data.Text
import CodeWorld
main :: Program
main = program
type Program = IO ()
program :: Program
program = etap5

etap5 :: IO ()
etap5 = runActivity $
        resettable $
        withStartScreen $
        withUndo $
        Activity (initialState 0) handleEvent draw

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D                    deriving Eq
data Coord = C Integer Integer                    deriving Eq

data State = S {
  playerDirection :: Direction,
  playerPosition  :: Coord,
  boxesCoords     :: [Coord],
  currentLevel    :: Integer,
  movesMade       :: Integer
} deriving Eq
data SSState world = StartScreen | Running world
data WithUndo a = WithUndo a [a]
data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   ::(world -> Picture)
}

wall :: Picture
wall = colored gray tile
ground :: Picture
ground = colored (light yellow) tile
storage :: Picture
storage = (colored pink (solidCircle 0.15)) & ground
box :: Picture
box = colored (light brown) tile
boxInStorage :: Picture
boxInStorage = colored brown tile
tile :: Picture
tile = solidRectangle 1 1

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

player1 :: Picture
player1 = solidPolygon [(-0.5, -0.5), (0, 0.5), (0.5, -0.5)]

directionToRadians :: Direction -> Double
directionToRadians U = 0
directionToRadians L = pi / 2
directionToRadians D = pi
directionToRadians R = 3 * pi / 2

playerRotated :: Direction -> Picture
playerRotated direction = rotated (directionToRadians direction) player1

getNewCoord :: Coord -> Direction -> Coord
getNewCoord (C x y) U = C x (y + 1)
getNewCoord (C x y) L = C (x - 1) y
getNewCoord (C x y) D = C x (y - 1)
getNewCoord (C x y) R = C (x + 1) y

isPassable :: Tile -> Bool
isPassable Ground    = True
isPassable Storage   = True
isPassable otherwise = False

startScreen :: Picture
startScreen = (scaled 3 3 $ lettering $ pack "Sokoban!") &
  (translated 0 (-2) $ lettering $ pack "Press N to skip the current level") &
  (translated 0 (-4) $ lettering $ pack "Press U to undo your move") &
  (translated 0 (-6) $ lettering $ pack "Press space bar to play")

winningScreen :: Integer -> Picture
winningScreen n = (lettering $ pack $
  "Level finished in " ++ show n ++ " moves!") &
  (translated 0 (-5) $ lettering $ pack "Press space bar for next level!")

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == pack "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) =
  Activity state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
         | key == pack " "             = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == pack "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

data Maze = Maze Coord (Coord -> Tile)

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

elemList :: Eq a => a -> [a] -> Bool
elemList _ []    = False
elemList e (h:t) = h == e || elemList e t

appendList :: [a] -> [a] -> [a]
appendList []    l2 = l2
appendList (h:t) l2 = h:(appendList t l2)

listLength :: [a] -> Integer
listLength []    = 0
listLength (_:t) = 1 + listLength t

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ []    = []
filterList f (h:t) = if f h then h:t' else t'
  where t' = filterList f t

nth :: [a] -> Integer -> a
nth l 0 = Prelude.head l
nth l n = nth (Prelude.tail l) (n - 1)

mapList :: (a -> b) -> [a] -> [b]
mapList _ []    = []
mapList f (h:t) = (f h):(mapList f t)

andList :: [Bool] -> Bool
andList []    = True
andList (h:t) = h && andList t

allList :: (a -> Bool) -> [a] -> Bool
allList f l = andList $ mapList (\x -> f x) l

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ e []    = e
foldList f e (h:t) = f h (foldList f e t)

getAllReachable :: Eq a => a -> (a -> [a]) -> [a]
getAllReachable initial neighbours =
  dfs [] initial neighbours
  where
  dfs visited initial neighbours =
    if elemList initial visited then visited
    else
      foldList
        (\v l -> dfs l v neighbours)
        (initial:visited)
        (neighbours initial)

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
  elemList v $ getAllReachable initial neighbours

graphFromMaze :: Maze -> Coord -> [Coord]
graphFromMaze (Maze _ tiles) (C x y) =
  filterList
    (\coord -> tiles coord /= Wall)
    [C x (y + 1), C (x + 1) y, C x (y - 1), C (x - 1) y]

getMazeSize :: Maze -> Integer
getMazeSize maze@(Maze _ tiles) =
  (findSize 1) - 1
  where
    findSize n =
      if (filterList 
          (\coord -> tiles coord /= Blank)
          [C n 0, C 0 n, C (-n) 0, C 0 (-n)]) /= []
      then findSize (n + 1)
      else n

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

initialBoxes :: Maze -> [Coord]
initialBoxes maze@(Maze initial tiles) =
  [C x y | x <- [-size..size], y <- [-size..size],
    (tiles $ C x y) == Box, reachable (C x y) initial graph]
  where
    size  = getMazeSize maze
    graph = graphFromMaze maze

initialState :: Integer -> State
initialState n =
  S { playerDirection = U, 
      playerPosition  = initial,
      boxesCoords     = initialBoxes maze,
      currentLevel    = n,
      movesMade       = 0 }
  where maze@(Maze initial _) = nth mazes n

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes [] = blank
pictureOfBoxes ((C x y):t) =
  (translated (fromIntegral x) (fromIntegral y) (drawTile Box)) & (pictureOfBoxes t)

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

pictureOfMaze :: Maze -> Picture
pictureOfMaze maze =
  pictures [translated (fromIntegral x) (fromIntegral y) $ drawTile $ tiles $ C x y
    | x <- [-size..size], y <- [-size..size]]
  where
    (Maze _ tiles) = removeBoxes maze
    size           = getMazeSize maze

picturesOfMazes = mapList pictureOfMaze mazes

draw :: State -> Picture
draw state =
  if isWinning state then winningScreen $ movesMade state
  else
    translated (fromIntegral x) (fromIntegral y) player &
    currentPictureOfBoxes &
    pictureOfCurrentMaze
  where
    currentPictureOfBoxes = pictureOfBoxes $ boxesCoords state
    pictureOfCurrentMaze = nth picturesOfMazes $ currentLevel state
    player = playerRotated $ playerDirection state
    C x y = playerPosition state

isWinning :: State -> Bool
isWinning state =
  allList
    (\coord -> tiles coord == Storage)
    (boxesCoords state)
  where (Maze _ tiles) = nth mazes $ currentLevel state

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state =
  let
    nextLevel =
      if listLength mazes > currentLevel state + 1 then
        initialState $ currentLevel state + 1
      else
        state
  in
  if isWinning state then
    if key == pack " " then nextLevel
    else state
  else if key == pack "N" then nextLevel
  else let
    currentCoord = playerPosition state
    maybeDirection =
      if      key == pack "Up"    then Just U
      else if key == pack "Left"  then Just L
      else if key == pack "Down"  then Just D
      else if key == pack "Right" then Just R
      else                        Nothing
  in case maybeDirection of
    Nothing -> state
    Just direction ->
      let
        nextCoord1@(C x1 y1) = getNewCoord currentCoord direction
        nextCoord2           = getNewCoord nextCoord1   direction
        (Maze _ currentMaze) = addBoxes (boxesCoords state) (removeBoxes $ nth mazes $ currentLevel state)
        state'               = state { playerDirection = direction, movesMade = 1 + movesMade state }
      in
      if isPassable $ currentMaze nextCoord1 then
        state' { playerPosition = nextCoord1 }
      else if (currentMaze nextCoord1 == Box) && (isPassable $ currentMaze nextCoord2) then
        state' { playerPosition = nextCoord1,
          boxesCoords = mapList (\coord -> if coord == nextCoord1 then nextCoord2 else coord) (boxesCoords state) }
      else state'
handleEvent otherwise state = state

