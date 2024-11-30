module Main where

type Grid = [[Bool]]

initialGrid :: Int -> Int -> Grid
initialGrid width height = replicate height (replicate width False)

printGrid :: Grid -> IO ()
printGrid =
  mapM_ (putStrLn . map (\cell -> if cell then '█' else ' '))

main :: IO ()
main = do
  let grid = initialGrid 20 20
  printGrid grid
