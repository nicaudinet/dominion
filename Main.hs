{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Numeric.Natural


-- * Type classes

class IsCard a where
  price :: a -> Price

class IsTreasure a where
  treasure :: a -> Treasure

class IsVictory a where
  victory :: a -> Victory


-- * Game values

newtype Price = Price Natural
  deriving Num

newtype Treasure = Treasure Natural
  deriving Num

newtype Victory = Victory Natural
  deriving Num

newtype Actions = Actions Natural
  deriving Num

newtype Buys = Buys Natural
  deriving Num


-- * Game types

type Deck = forall a. IsCard a => [a]

type Board = forall a. IsCard a => [(Int, a)]


-- * Treasure cards

data Copper = Copper
instance IsCard Copper where
  price Copper = 0
instance IsTreasure Copper where
  treasure Copper = 1

data Silver = Silver
instance IsCard Silver where
  price Silver = 3
instance IsTreasure Silver where
  treasure Silver = 2

data Gold = Gold
instance IsCard Gold where
  price Gold = 6
instance IsTreasure Gold where
  treasure Gold = 3


-- * Victory cards

data Estate = Estate
instance IsCard Estate where
  price Estate = 2
instance IsVictory Estate where
  victory Estate = 1

data Dutchy = Dutchy
instance IsCard Dutchy where
  price Dutchy = 5
instance IsVictory Dutchy where
  victory Dutchy = 3

data Province = Province
instance IsCard Province where
  price Province = 8
instance IsVictory Province where
  victory Province = 6


-- * Action cards

data Villager = Villager
instance IsCard Villager where
  price Villager = 3

data Smithy = Smithy
instance IsCard Smithy where
  price Smithy = 4

data Mine = Mine
instance IsCard Mine where
  price Mine = 5

data Market = Market
instance IsCard Market where
  price Market = 5

data Cellar = Cellar
instance IsCard Cellar where
  price Cellar = 2

data Chapel = Chapel
instance IsCard Chapel where
  price Chapel = 2

-- * Main

main :: IO ()
main = putStrLn "Hello, Haskell!"
