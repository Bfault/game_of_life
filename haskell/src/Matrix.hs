module Matrix
( Matrix(..)
, createMatrix
-- , randomizeMatrix
) where

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

createMatrix :: Int -> Int -> a -> Matrix a
createMatrix width height default_ = Matrix {
    rows = width,
    cols = height,
    data_ = replicate height (replicate width default_)
}

-- randomizeMatrix :: (Random a) => Int -> Int -> (a, a) -> IO (Matrix a)