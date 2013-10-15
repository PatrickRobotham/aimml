{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -XTypeFamilies -XRankNTypes -XFlexibleContexts -XGADTs #-}
{-# OPTIONS -XScopedTypeVariables #-}

module World where
import Util
import Environment
import Model
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Data.Map (Map)
import Text.Show
import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v


data Agent = Agent {
  age :: Int,
  totalReward :: Reward,
  horizon :: Int,
  learningPeriod :: Int,
  simulations :: Int
  } deriving (Show, Eq)

data NodeType = Chance | Decision deriving (Show,Eq)

type SearchTree = HashTable [Bool] SearchNode -- Use HashTable Instead?

data SearchNode = SearchNode {
  rewardEstimate :: Double,
  visits :: Int,
  nodeType :: NodeType
  } deriving (Show, Eq)

data WorldState = WorldState {
  env :: EnvironmentP,
  agent :: Agent,
  model :: ModelP,
  tree :: SearchTree,
  gen :: StdGen,
  history :: [Bool],
  reward :: Integer,
  sampleReward :: Double
  }

type WorldStateT = StateT WorldState

type World a = (MonadIO m) => WorldStateT m a
