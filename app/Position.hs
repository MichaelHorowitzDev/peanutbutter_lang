module Position where

data Position = Position {
    posOffset :: Int,
    posLength :: Int
    } deriving (Eq, Ord, Show)