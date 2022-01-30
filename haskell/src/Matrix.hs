module Matrix
( Matrix(..)
, createMatrix
, randomizeMatrix
) where

import System.Random

data Matrix a = Matrix {
    rows :: Int,
    cols :: Int,
    data_ :: [[a]]
}

instance (Show a) => Show (Matrix a) where
    show matrice =
        let
            showRow :: (Show a) => [a] -> String
            showRow row = unwords $ map show row
        in
            unlines $ map showRow (data_ matrice)

instance Functor Matrix where
    fmap f (Matrix rows cols data_) =
        let
            fmapRow :: (a -> b) -> [a] -> [b]
            fmapRow f row = map f row
        in
            Matrix rows cols $ map (fmapRow f) data_

instance (Random a) => Random Matrix a where

createMatrix :: Int -> Int -> a -> Matrix a
createMatrix width height default_ = Matrix {
    rows = width,
    cols = height,
    data_ = replicate height (replicate width default_)
}


-- TODO: create an instance of random
randomizeMatrix :: (Random a) => StdGen -> Matrix a -> Matrix a
randomizeMatrix stdGen matrice =
    let generateRandomRow :: (Random a) => StdGen -> ([a], StdGen)
        generateRandomRow gen = random (mkStdGen stdGen) :: ([a], newStdGen)
    in fmap generateRandomRow matrice