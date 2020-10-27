module MyTest where

data MyDataType = Thing Int | Blah

foo :: MyDataType -> Maybe Int
foo (Thing n) = Just (3 * n)
foo Blah = Nothing
