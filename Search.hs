{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -XTypeFamilies -XRankNTypes -XFlexibleContexts -XGADTs #-}
{-# OPTIONS -XScopedTypeVariables #-}

-- Performance Notes:
-- Replace Map with HashTable
-- Replace History with Dequeue

module Search where
import Util
import World
import Model
import Environment
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity ( )
import System.Random (StdGen, randomR)
import Data.Map (Map, findWithDefault, insertWith, insert, empty)
import Data.BitVector
import Data.List (maximumBy)
import Data.Random (sampleState)
import Data.Random.Extras (choice)

explorationConstant = 2.0

--------------------------------------------------------------------------------
-- Utility Functions

modifyModel :: (ModelP -> ModelP) -> World ()
modifyModel f = do
  m <- gets model
  modify (\x -> x{model = f m})

modifyEnvironment :: (EnvironmentP -> EnvironmentP) -> World ()
modifyEnvironment f = do
  e <- gets env
  modify (\x -> x{env = f e})

modifyTree :: (SearchTree -> SearchTree) -> World ()
modifyTree f = do
  tr <- gets tree
  modify (\x ->x{tree = f tr})

modifyReward :: (Integer -> Integer) -> World ()
modifyReward f = do
  r <- gets reward
  modify (\x -> x{reward = f r})

modifySampleReward :: (Reward -> Reward) -> World ()
modifySampleReward f = do
  r <- gets sampleReward
  modify (\x -> x{sampleReward = f r})

chanceNode :: SearchNode -> Bool
chanceNode SearchNode{nodeType = Chance} = True
chanceNode _ = False



maxBitsNeeded :: World Int
maxBitsNeeded = do
  e <- gets env
  return $ max (actionBits e) (perceptBits e)

genRandomAction :: World Action
genRandomAction = do
  g <- gets gen
  e <- gets env
  let (a,g1) = randomR (0, maxAction e) g
  modify (\x -> x{gen = g1})
  return a
                               
decodeAction :: [Bool] -> World Action
decodeAction bv = do
  e <- gets env
  return $ (mod ((nat.fromBits) bv) (maxAction e + 1))

decodeObservation :: [Bool] -> World Percept
decodeObservation bv = do
  e <- gets env
  return $ (mod (nat$fromBits bv) ((maxObservation) e + 1))

decodeReward :: [Bool] -> World Percept
decodeReward bv = do
  e <- gets env
  return $ (mod (nat $ fromBits bv) ((maxReward) e + 1))

decodePercept :: [Bool] -> World (Percept, Percept)
decodePercept bv = do
  e <- gets env
  let (o,r) = splitAt (observationBits e) bv
  o1 <- decodeObservation o
  r1 <- decodeReward r
  return (o1, r1)

encodeAction :: Action -> World [Bool]
encodeAction act = do
  e <- gets env
  return $ toBits (bitVec (actionBits e) act)


encodeActionP :: (Environment e) => e -> Action -> [Bool]
encodeActionP env act = toBits (bitVec (actionBits env) act)

encodePercept :: (Percept,Percept) -> World [Bool]
encodePercept (o,r) = do
  e <- gets env
  return $ 
    toBits (bitVec (observationBits e) o) ++
    toBits (bitVec (rewardBits e) r)

makeAgent :: Options -> Agent
makeAgent opts = 
  Agent{age = 0,
        totalReward = 0,
        horizon = read $ getRequiredOption "agent-horizon" opts,
        learningPeriod = read $ findWithDefault "0" "learning-period" opts,
        simulations = read $ getRequiredOption "mc-simulations" opts
       }

averageReward :: Agent -> Double
averageReward x = if age x > 0
                  then (totalReward x) / (fromIntegral $ age x)
                  else 0



newSearchNode :: NodeType -> SearchNode
newSearchNode t = SearchNode {
  rewardEstimate = 0,
  visits = 0,
  nodeType = t
  }

updateNodePercept :: (Percept,Percept) -> World ()
-- Modifies the search tree
updateNodePercept (o,r) = do
  or <- encodePercept (o,r)
  h <- gets history
  tr <- gets tree
  let hor = h ++ or
  let n = findWithDefault (newSearchNode Decision) hor tr
  modify (\x -> x{tree = insertWith (flip const) hor n tr,
                  history = hor})
  return ()

updateNodeAction :: Action -> World ()
-- Modifies the search tree
updateNodeAction a = do
  act <- encodeAction a
  h <- gets history
  tr <- gets tree
  let ha = h ++ act
  let n = findWithDefault (newSearchNode Chance) ha tr
  modify (\x -> x{tree = insertWith (flip const) ha n tr,
                  history = ha})
  return ()

updateModelAction :: Action -> World ()
updateModelAction a = do
  bits <- encodeAction a
  m <- gets model
  modify (\x -> x{model = updateHistory m bits})
  
updateModelPercept :: (Percept,Percept) -> World()
updateModelPercept (o,r) = do
  bits <- encodePercept (o,r)
  modifyModel (updateList bits)

genPercept :: World (Percept,Percept)
genPercept = do
  e <- gets env
  t0 <- gets model
  g0 <- gets gen
  let (bv, g1, t1) = genRandomSymbols t0 g0 (perceptBits e)
  (o,r) <- decodePercept bv
  modify (\x -> x{model = t1, gen = g1})
  return (o,r)


argmax :: (Ord b) => (a->b) -> [a] -> a -- Implement random tie-breaking?
argmax f lst = maximumBy (\x y -> compare (f x) (f y)) lst


getRandom :: [a] -> World a
getRandom l = do
  g <- gets gen
  let (x,g1) = sampleState (choice l) g
  modify (\x -> x{gen = g1})
  return x


tee :: [Bool] -> World Int
tee h = do
  tr <- gets tree
  let n = findWithDefault (newSearchNode Decision) h tr
  return $ visits n

veehat h = do
  tr <- gets tree
  let n = findWithDefault (newSearchNode Decision) h tr
  return $ rewardEstimate n

--------------------------------------------------------------------------------
-- The Main Algorithm




selectAction :: World Action
selectAction = do
  e <- gets env
  tr <- gets tree
  h <- gets history
  let n = findWithDefault (newSearchNode Decision) h tr
  let actions = [minAction e .. maxAction e]
  let getNode :: Action -> SearchNode
      getNode a = findWithDefault (newSearchNode Chance) 
                  (h ++ encodeActionP e a) tr
  let u = [a | a <- actions ,  (visits $ getNode a) == 0]
  let estval :: Action -> Double
      estval x = (rewardEstimate.getNode) x
                 + explorationConstant
                 * sqrt((log.fromIntegral.visits) n
                        /(log.fromIntegral.visits.getNode) x)
  if not (null u) 
    then getRandom u
    else return$ argmax estval actions
rollout :: Int -> World Reward
rollout 0 = do
  r <- gets reward
  modify (\x -> x{reward = 0})
  return $ fromIntegral r

rollout n = do
  a <- genRandomAction
  updateModelAction a
  (o,r) <- genPercept
  modifyReward (+r)
  rollout (n - 1)


sample :: Int -> World Reward
sample 0 = return 0
sample n = do
  h <- gets history
  tr <- gets tree
  let nh = findWithDefault (newSearchNode Decision) h tr
  when (chanceNode nh) (
    do
      (o,r) <- genPercept
      updateNodePercept (o,r)
      r1 <- sample (n-1)
      modifySampleReward (\x -> r1 + fromIntegral r)
      )
  when (visits nh == 0) (
    do
      r1 <- rollout n
      modifySampleReward (\x -> r1)
    )
  when (not (chanceNode nh || visits nh == 0)) (
    do
      a <- selectAction
      updateModelAction a
      updateNodeAction a
      r1 <- sample (n-1)
      modifySampleReward(\x -> r1)
      )
  rew <- gets sampleReward
  vh <- veehat h
  th <- tee h
  let th' = fromIntegral th
  let nh' = nh{rewardEstimate = 1/(th' + 1) * (rew + th' * vh),
               visits = th + 1}
  modifyTree (\x -> insert h nh' x)
  return rew


search :: World Action  
search = do
  -- Assume that search is called when history is on an action
  m <- gets model
  h <- gets (getHistory.model)
  hz <- gets (horizon.agent)
  n <- gets (simulations.agent)
  e <- gets env
  modify(\x -> x{tree = insert h (newSearchNode Chance) empty,
                 history = h})
  -- Sample Repeatedly
  forM_ [1.. n] (\_ ->
    do
      sample hz;
      modify (\x -> x{model = m, history = reverse h})
    )
  tr <- gets tree
  -- Find Best Action
  let getEst a =
        rewardEstimate $ 
        findWithDefault (newSearchNode Chance) (h ++ encodeActionP e a) tr
  let actions = [0 .. maxAction e]
  return $ argmax getEst actions