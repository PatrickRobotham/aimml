module Coinflip (CoinFlip)
where
import Environment
import System.Random
import Data.Map
import Util

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
    let 
      prob :: Double
      prob = read $ findWithDefault (show cDefaultProbability) "coin-flip-p" o
      s :: StdGen
      s = read $ findWithDefault ((show.mkStdGen) 0) "coin-flip-seed" o
      (toss,sd) = randomR (0.0,1.0) s
    in if not (0 <= prob && prob <= 1)
       then error "Invalid Probability"
       else Flipper{
         seed = sd,
         action = -1, -- No action read yet
         reward = 0,
         observation = bool2Int (toss < prob),
         probability = prob
         }


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
cDefaultProbability = 1
bool2Int False = 0
bool2Int True = 1