{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Numeric.Natural


-- * Helper material

-- ** Combining data types

data a + b = InL a | InR b
infixr 8 +

instance (IsCard a, IsCard b) => IsCard (a + b) where
  price (InL a) = price a
  price (InR b) = price b

-- ** Injection

class a :<: b where
  inj :: a -> b

instance a :<: a where
  inj = id

instance a :<: (a + b) where
  inj = InL

instance {-# OVERLAPPABLE #-} (a :<: c) => a :<: (b + c) where
  inj = InR . inj


-- * Type Classes

-- | Properties of every card
class IsCard a where
  price :: a -> Natural


-- * Card definitions

type Card = Treasure + Victory + Action

-- ** Treasure Cards

data Treasure
  = Copper
  | Silver
  | Gold

instance IsCard Treasure where
  price Copper = 0
  price Silver = 3
  price Gold = 6

-- ** Victory Cards

data Victory
  = Estate
  | Duchy
  | Province

instance IsCard Victory where
  price Estate = 2
  price Duchy = 5
  price Province = 8

-- ** Action Cards

data Action
  = Cellar
  | Chapel
  | Village
  | Smithy
  | Market
  | Mine

instance IsCard Action where
  price Cellar = 2
  price Chapel = 2
  price Village = 3
  price Smithy = 4
  price Market = 5
  price Mine = 5


-- * Deck definition

type Deck = [Card]

startDeck :: Deck
startDeck = replicate 3 (inj Estate) <> replicate 7 (inj Copper)


-- * Main function

main :: IO ()
main = putStrLn "Hello, Haskell!"
