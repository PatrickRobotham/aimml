module Coinflip (CoinFlip)
where
import Environment
import System.Random
import Data.Map
import Typedefs

instance Show CoinFlip where
  show c = 
    "prediction: " ++ (if action c == aTails then "tails" else "heads") 
    ++ ", observation: " ++ (if observation c == oTails 
                             then "tails" else "heads")
    ++ ", reward: " ++ (show.reward) c ++ "\n"
    
instance Environment CoinFlip where
  performAction act c =
    let (toss, s1) = randomR (0.0,1.0) (seed c)
        (ob, rew) = if (toss < (probability c))  
                    then (oHeads, if act == oHeads then rWin else rLoss)
                    else (oTails, if act == oTails then rWin else rLoss)
    in c{observation = ob, reward = rew, seed = s1, action = act}

  getObservation = observation
  getReward = reward
  maxAction _ = 1
  maxObservation _ = 1
  maxReward _ = 1
  makeNewEnvironment o =
    error "fill in"


data CoinFlip = Flipper{
  seed :: StdGen,
  action :: Action,
  reward :: Percept,
  observation :: Percept,
  probability :: Double
  } 
                
aTails = 0
aHeads = 1
oTails = 0
oHeads = 1
rLoss = 0
rWin = 1
cDefaultProbability = 0.7