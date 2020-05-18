{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Exception.Safe
import Control.Monad.State
import Data.Generics.Product
import Data.List
import Data.Ord
import Data.Void
import GHC.Generics
import Lens.Micro
import Lens.Micro.Extras
import Numeric.Natural
import System.Random


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

-- ** Injectable

class Injectable (a :: *) (as :: [*]) where
  inj :: a -> Summed as

instance Injectable a (a ': as) where
  inj = InL

instance {-# OVERLAPPABLE #-} Injectable a as => Injectable a (b ': as) where
  inj = InR . inj

-- ** Extractable

class Extractable (a :: *) (as :: [*]) where
  ext :: Summed as -> Maybe a

instance Extractable a (a ': as) where
  ext (InL a) = Just a
  ext (InR _) = Nothing

instance {-# OVERLAPPABLE #-} Extractable a as => Extractable a (b ': as) where
  ext (InL _) = Nothing
  ext (InR as) = ext as

-- ** PartOf

type a :<: b = (Injectable a b, Extractable a b)
infixr 8 :<:


-- * Type Classes

-- | Properties of every card
class a :<: CardList => IsCard a where
  price :: a -> Natural

class Drawable pile where
  type Draw pile :: *
  draw :: pile -> Draw pile


-- * Card definitions

type CardList = '[Treasure, Victory, Action]
type Card = Summed CardList

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
  { playerDeck :: Deck
  , playerDiscard :: Deck
  }

instance Drawable Player where
  type Draw Player = IO (Card, Player)
  draw (Player [] []) = throwIO PlayerHasNoCards
  draw (Player [] discard) = do
    newDeck <- shuffle discard
    draw (Player newDeck [])
  draw (Player (card : remainder) discard) = do
    pure (card, Player remainder discard)

startPlayer :: Player
startPlayer = Player startDeck []

shuffle :: [a] -> IO [a]
shuffle xs = do
  ns <- replicateM (length xs) (randomRIO (minBound :: Int, maxBound))
  pure (map snd (sortBy (comparing fst) (zip ns xs)))

data Players = Two { current :: Player, other :: Player }
  deriving Generic

-- ** Board definition

data Pile = Pile
  { pileCard :: Card
  , pileSize :: Natural
  }

instance Drawable Pile where
  type Draw Pile = Maybe Card
  draw (Pile _ 0) = Nothing
  draw (Pile c _) = Just c

type Board = [Pile]

pile :: IsCard a => a -> Natural -> Pile
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


-- * Turn

data TurnTracker = TurnTracker
  { hand :: [Card]
  , playedCards :: [Card]
  , actions :: Natural
  , buys :: Natural
  }

initTurn :: [Card] -> TurnTracker
initTurn hand = TurnTracker hand [] 1 1

card :: IsCard a => Card -> a
card = maybe (error "tried to match a type which is not a card") id . ext

purchasePower :: Card -> Natural
purchasePower (card -> Gold) = 3
purchasePower (card -> Silver) = 2
purchasePower (card -> Copper) = 1
purchasePower _ = 0


-- * Lenses

currentPlayer :: Lens' GameState Player
currentPlayer = field @"players" . field @"current"



-- * Game

newtype Game a = Game (StateT GameState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState GameState, MonadIO)

data GameState = GameState
  { players :: Players
  , board :: Board
  }
  deriving Generic

startGameState :: GameState
startGameState = GameState (Two startPlayer startPlayer) startBoard

drawHand :: Game [Card]
drawHand = do
  player <- gets (view currentPlayer)
  (hand, newPlayer) <- liftIO $ runStateT (replicateM 5 go) player
  modify (over currentPlayer (const newPlayer))
  pure hand
  where
    go :: StateT Player IO Card
    go = do
      (card, newPlayer) <- liftIO . draw =<< get
      put newPlayer
      pure card

playHand :: StateT [Card] Game ()
playHand = do
  actionPhase
  buyPhase

actionPhase :: StateT [Card] Game ()
actionPhase = undefined

buyPhase :: StateT [Card] Game ()
buyPhase = undefined

turn :: Game ()
turn = drawHand >>= evalStateT playHand

runGame :: Game a -> IO (a, GameState)
runGame (Game game) = runStateT game startGameState


-- * Exceptions

data PlayerHasNoCards = PlayerHasNoCards
  deriving (Show, Exception)


-- * Main function

main :: IO ()
main = putStrLn "Hello, Haskell!"
