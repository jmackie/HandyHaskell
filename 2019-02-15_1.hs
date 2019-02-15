module Main where

import Data.List (sort)
import Data.Ord (comparing)


main :: IO ()
main = print . sort $
    [ Person "A" "B"
    , Person "A" "A"
    , Person "B" "C"
    , Person "B" "A"
    ]
--  [ Person "A" "A"
--  , Person "A" "B"
--  , Person "B" "A"
--  , Person "B" "C"
--  ]

data Person = Person
    { firstName  :: String
    , lastName   :: String
    } deriving (Show, Eq)

instance Ord Person where
    -- Semigroup b => Semigroup (a -> b)
    compare = comparing firstName <> comparing lastName

--instance Semigroup Ordering where
--    LT <> _ = LT
--    EQ <> y = y
--    GT <> _ = GT
