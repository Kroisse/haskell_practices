module Main where

infixr 5 :::

data CList a = CNil
             | a ::: (CList a)
    deriving (Show)

toList CNil = []
toList (x ::: rs) = x : toList rs
fromList [] = CNil
fromList (x:rs) = x ::: fromList rs