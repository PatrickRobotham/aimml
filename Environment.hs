{-# LANGUAGE ExistentialQuantification #-}
module Environment where
import Util
import Data.BitVector

class (Show e) => Environment e where   
      performAction :: Action -> e -> e             
      isFinished :: e -> Bool
      isFinished _ = False
      getObservation :: e -> Percept    
      getReward :: e -> Percept          
      -- The number of bits required to represent an action
      actionBits :: e -> Int         
      actionBits e = integerWidth $ maxAction e
      -- The number of bits required to represent an observation 
      observationBits:: e -> Int
      observationBits e = integerWidth $ maxObservation e
      rewardBits :: e -> Int 
      rewardBits e = integerWidth $ maxReward e
      perceptBits :: e -> Int
      perceptBits e = observationBits e + rewardBits e
      maxAction :: e -> Action
      maxAction _ = 0
      maxObservation :: e -> Percept
      maxObservation _ = 0
      maxReward :: e -> Percept
      maxReward _ = 0
      minAction :: e -> Action
      minAction _ = 0
      minObservation :: e -> Percept
      minObservation _ = 0
      minReward :: e -> Percept
      minReward _ = 0
      isValidAction :: e -> Action -> Bool
      isValidAction e act = (minAction e) <= act && act <= (maxAction e)
      isValidObservation :: e -> Percept -> Bool
      isValidObservation e per = 
        (minObservation e) <= per && per <= (maxObservation e)
      isValidReward :: e -> Percept -> Bool
      isValidReward e per = (minReward e) <= per && per <= (maxReward e)
      makeNewEnvironment :: Options -> e
      

data EnvironmentP = forall a. Environment a => EnvironmentP a 

instance Show EnvironmentP where
  show (EnvironmentP e) = show e
  
instance Environment EnvironmentP where
  performAction a (EnvironmentP e) = EnvironmentP (performAction a e)
  isFinished (EnvironmentP e) = isFinished e
  getObservation (EnvironmentP e) = getObservation e
  getReward (EnvironmentP e) = getReward e
  actionBits (EnvironmentP e) = actionBits e
  observationBits (EnvironmentP e) = observationBits e
  rewardBits (EnvironmentP e) = rewardBits e
  perceptBits (EnvironmentP e) = perceptBits e
  maxAction (EnvironmentP e) = maxAction e
  maxObservation (EnvironmentP e) = maxObservation e
  maxReward (EnvironmentP e) = maxReward e
  minAction (EnvironmentP e) = minAction e
  minObservation (EnvironmentP e) = minObservation e
  minReward (EnvironmentP e) = minReward e
  isValidAction (EnvironmentP e) = isValidAction e
  isValidObservation (EnvironmentP e) = isValidObservation e
  isValidReward (EnvironmentP e) = isValidReward e
  makeNewEnvironment opts = error "EnvironmentP is an abstract type"