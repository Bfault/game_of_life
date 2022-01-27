module Main where

import System.Environment
import System.IO
import System.Random

import Matrix

createMap :: Int -> Matrix Bool
createMap n = createMatrix (n * 2) n False

main :: IO ()
main = do
  args <- getArgs
  print $ createMap (read (head args) :: Int)
