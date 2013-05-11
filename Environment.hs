module Environment where
import Typedefs
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