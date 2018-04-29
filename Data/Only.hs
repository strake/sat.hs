module Data.Only where

newtype Only a = Only { only :: a } deriving (Eq, Show)
instance Eq a => Semigroup (Only a) where a <> b | a == b = a | otherwise = error "Internal contradiction"
