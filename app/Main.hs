module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)

type Width = Int

type Height = Int

data CellState = Dead | Alive 
 deriving (Eq)

type Grid = [[CellState]]

initialGrid :: Width -> Height -> IO Grid
initialGrid width height =
  replicateM height ( replicateM width (fmap (\x -> if x then Alive else Dead) (randomRIO(True, False))))

getDimensions :: Grid -> (Width, Height)
getDimensions g = (length g, length $ head g)

printGrid :: Grid -> IO ()
printGrid =
  mapM_ (putStrLn . map (\cell -> if cell == Alive then '█' else ' '))

countNeighbors :: Grid -> Int -> Int -> Int
countNeighbors grid row col =
  sum
    [ if isAlive row' col' then 1 else 0
      | row' <- [row - 1 .. row + 1],
        col' <- [col - 1 .. col + 1],
        (row', col') /= (row, col) -- Skip center cell itself.
    ]
  where
    (width, height) = getDimensions grid
    isAlive r c = r >= 0 && r < height && c >= 0 && c < width && grid !! r !! c == Alive

updateCell :: Grid -> Int -> Int -> CellState
updateCell g x y
  | not isAlive && nNeighbors == 3 = Alive
  | isAlive && nNeighbors == 2 = Alive
  | isAlive && nNeighbors == 3 = Alive
  | otherwise = Dead
  where
    isAlive = g !! x !! y == Alive
    nNeighbors = countNeighbors g x y

updateGrid :: Grid -> Grid
updateGrid grid =
  let (width, height) = getDimensions grid
   in [[updateCell grid row col | col <- [0 .. width - 1]] | row <- [0 .. height - 1]]

gameLoop :: Grid -> IO ()
gameLoop grid = do
  clearScreen
  printGrid grid
  threadDelay 250000 -- 0.25 second delay
  gameLoop (updateGrid grid)

main :: IO ()
main = do
  grid <- initialGrid 40 40
  gameLoop grid
