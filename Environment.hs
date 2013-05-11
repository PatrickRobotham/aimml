module Environment where
-- Make Environment an instance of Show
import Typedefs


class (Show e) => Environment e where   
      performAction :: Action -> e              
      isFinished :: e -> Bool
      getObservation :: e -> Percept    
      getReward :: e -> Reward          
      -- The number of bits required to represent an action
      actionBits :: e -> Int         
      -- The number of bits required to represent an observation 
      observationBits:: e -> Int
      rewardBits :: e -> Int 
      perceptBits :: e -> Int
      maxAction :: e -> Action
      maxObservation :: e -> Percept
      maxReward :: e -> Percept
      minAction :: e -> Action
      minObservation :: e -> Percept
      minReward :: e -> Percept
      isValidAction :: e -> Action -> Bool
      isValidObservation :: e -> Percept -> Bool
      isValidReward :: e -> Percept -> Bool
