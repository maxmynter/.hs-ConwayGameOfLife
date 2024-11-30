module Main where

import Control.Monad (replicateM)
import System.Random (randomRIO)

type Grid = [[Bool]]

initialGrid :: Int -> Int -> IO Grid
initialGrid width height =
  replicateM height (replicateM width (randomRIO (False, True)))

printGrid :: Grid -> IO ()
printGrid =
  mapM_ (putStrLn . map (\cell -> if cell then '█' else ' '))

main :: IO ()
main = do
  grid <- initialGrid 20 20
  printGrid grid
