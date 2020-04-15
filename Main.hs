{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.State
import Data.Void
import Numeric.Natural


-- * Helper material

-- ** Summable

class Summable (as :: [*]) where
  data Summed as :: *

instance Summable '[] where
  data Summed '[] = Void

instance Summable (a ': as) where
  data Summed (a ': as)
    = InL a
    | InR (Summed as)

class Injectable (a :: *) (as :: [*]) where
  inj :: a -> Summed as

instance Injectable a (a ': as) where
  inj = InL

instance {-# OVERLAPPABLE #-} Injectable a as => Injectable a (b ': as) where
  inj = InR . inj


-- * Type Classes

-- | Properties of every card
class (Injectable a CardTypes) => IsCard a where
  price :: a -> Natural


-- * Card definitions

type CardTypes = '[Treasure, Victory, Action]
type Card = Summed CardTypes

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


-- * Game state types

-- ** Deck definition

type Deck = [Card]

startDeck :: Deck
startDeck = replicate 3 (inj Estate) <> replicate 7 (inj Copper)

-- ** Player definition

data Player = Player
  { deck :: Deck
  , discard :: Deck
  }

startPlayer :: Player
startPlayer = Player startDeck []

-- ** Board definition

data Pile = Pile
  { card :: Card
  , size :: Natural
  }

type Board = [Pile]

pile :: Injectable a CardTypes => a -> Natural -> Pile
pile card = Pile (inj card)

startBoard :: Board
startBoard =
  -- Treasure cards
  [ pile Gold 20
  , pile Silver 20
  , pile Copper 20
  -- Victory cards
  , pile Estate 15
  , pile Duchy 15
  , pile Province 15
  -- Actions
  , pile Cellar 10
  , pile Chapel 10
  , pile Village 10
  , pile Smithy 10
  , pile Market 10
  , pile Mine 10
  ]


-- * Game

data GameState = GameState
  { players :: [Player]
  , board :: Board
  }

startGameState :: Natural -> GameState
startGameState n =
  let players = replicate (fromIntegral n) startPlayer
  in GameState players startBoard

type Game a = State GameState a

runGame :: Natural -> Game a -> (a, GameState)
runGame n = flip runState (startGameState n)


-- * Main function

main :: IO ()
main = putStrLn "Hello, Haskell!"
