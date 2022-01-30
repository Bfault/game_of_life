module Main where

import System.Environment
import System.IO

import Matrix

createMap :: Int -> Matrix Bool
createMap n = randomizeMatrix 42 $ createMatrix (n * 2) n False

main :: IO ()
main = do
  args <- getArgs
  print $ createMap (read (head args) :: Int)
