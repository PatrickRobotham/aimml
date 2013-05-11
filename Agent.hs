{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -XTypeFamilies -XRankNTypes -XFlexibleContexts -XGADTs #-}
module Agent
where
import Environment
import Typedefs
import Predict hiding (visits)
import System.Random
import Data.BitVector
import Prelude hiding (lookup)
import Data.Map hiding (foldr, map)


maxBitsNeeded :: (Environment e) => e -> Int
maxBitsNeeded  env = max (actionBits env) (perceptBits env)

genRandomAction :: (RandomGen g, Environment e) => g -> e ->  (Action,g)
genRandomAction g env = randomR (0, maxAction env) g
                               
decodeAction :: (Environment e) => e -> [Bool] -> Action
decodeAction env bv = (mod ((nat.fromBits) bv) (maxAction env + 1))

decodeObservation :: (Environment e) => e -> [Bool] -> Percept
decodeObservation env bv =  (mod (nat$fromBits bv) ((maxObservation) env + 1))                            

decodeReward :: (Environment e) => e -> [Bool] -> Percept
decodeReward env bv = (mod (nat$fromBits bv) ((maxReward) env + 1))
decodePercept :: (Environment e) => e -> [Bool] -> (Percept, Percept)
decodePercept env bv =
  let (o,r) = splitAt (observationBits env) bv
  in (decodeObservation env o, decodeReward env r)

genAction :: (RandomGen g, Model m, Environment e) => g -> m -> e -> (Action,g)
genAction g m env =
  let (actbv, g1) = genRandomList m g (actionBits env)
  in (decodeAction env actbv, g1)

genPercept :: (RandomGen g, Model m, Environment e) => g -> m -> e -> 
              ((Percept, Percept), g)
genPercept g m env = 
 let (pbv, g1) = genRandomList m g (perceptBits env)
 in (decodePercept env pbv, g1)

encodeAction :: (Environment e) => e -> Action -> [Bool]
encodeAction env act = toBits (bitVec (actionBits env) act)

encodePercept :: (Environment e) => e -> (Percept,Percept) -> [Bool]
encodePercept env (o,r) = toBits (bitVec (observationBits env) o) ++
                          toBits (bitVec (rewardBits env) r)


-- We could make the model part of the agent
-- I chose not to do this in order to enforce
-- a cleaner seperation between the model
-- parts and the agent part.

class Agent a where
  makeAgent :: (Environment e, Model m) => AgentOptions -> e -> (m,a)
  age :: a -> Int
  totalReward :: a -> Reward
  averageReward :: a -> Double
  averageReward x = if age x > 0
                    then (totalReward x) / (fromIntegral $ age x)
                    else 0
  horizon :: a -> Int 
  -- The implementation of genRandomAction depends on the environment
  genPerceptAndUpdate :: (RandomGen g, Model m, Environment e) => 
                         g -> (m,a) -> e -> ((m,a),(Percept,Percept),g)
  modelUpdate :: (Model m, Environment e) => (m,a) -> e -> 
                 (Percept, Percept) -> (m,a)
  modelUpdate2 :: (Model m, Environment e) => (m,a) -> e -> Action -> (m,a)
  getPredictedActionProb :: (Model m, Environment e) => (m,a) -> 
                            e -> Action -> Double
  perceptProbability :: (Model m, Environment e) => 
                        (m,a) ->  e -> (Percept,Percept) -> Double
  search :: (Model m,RandomGen g, Environment e) => g -> a -> m -> e -> (Action,g)
  playout :: (Model m, RandomGen g, Environment e) => g -> 
             (m,a) -> e -> Int -> ((m,a),Reward,g) 
             
instance Agent AgentA where
  makeAgent  (AgentOptions{horizonO = h,
                           simulationsO = s,
                           learningPeriodO = l,
                           modelOptions = o}) env = 
    let ai = Agent{ageA = 0, totalRewardA = 0, horizonA = h,
                   learningPeriodA = l, simulationsA = s}
    in (makeNewModel o, ai)
                 
  age = ageA
  
  totalReward = totalRewardA
  
  horizon = horizonA
  
  genPerceptAndUpdate g (m,agent) env = 
    let (bv, g1, m1) = genRandomSymbolsAndUpdate m g (perceptBits env)
        (o, r) = decodePercept env bv
        ai = agent{totalRewardA = totalRewardA agent + fromIntegral r}
    in  ((m1, ai),(o,r), g1)
        
  modelUpdate (m, ai) env (o,r) =
    if (isValidObservation env o) && (isValidReward env r)  
    then let bv = encodePercept env (o,r)
             m1 = if (learningPeriodA ai > 0 && ageA ai > learningPeriodA ai)
                  then updateHistory m bv
                  else updateList m bv
             a1 = ai{totalRewardA = totalRewardA ai + fromIntegral r}       
         in  (m1, a1)
    else error "Invalid Percept. Cannot update." 
         
  modelUpdate2 (m, agent) env act = 
    if (isValidAction env act)
    then let bv = encodeAction env act
             m1 = updateHistory m bv
             a1 = agent{ageA = ageA agent + 1}
         in (m1,a1)
    else error "Invalid Action. Cannot update."
  
  getPredictedActionProb (m, agent) env act =
    if (isValidAction env act)
    then let bv = encodeAction env act
         in predictList m bv
    else error "Invalid Action. Cannot Predict."
            
  perceptProbability (m,agent) env  (o,r) =
    if (isValidObservation env o) && (isValidReward env r)  
    then let bv = encodePercept env (o,r)
         in predictList m bv
    else error "Invalid Percept. Cannot Predict."
  
  playout g (m, agent) env hz =
    playout1 g m agent env hz 0.0
    where
      playout1:: (RandomGen g, Model m, Environment e) =>
                 g -> m -> AgentA -> e -> Int ->  Reward -> ((m,AgentA),Reward,g)
      playout1 g m agent env 0 reward = ((m, agent), reward, g)
      playout1 g m agent env hz reward =
        let (act1, g1) = genRandomAction g env
            (m1,a1) = modelUpdate2 (m, agent) env act1
            ((m2,a2),(o,r),g2) = genPerceptAndUpdate g1 (m1,a1) env  
            reward2 = reward + (fromIntegral r)
        in playout1 g2 m2 a2 env (pred hz) reward2
  
  search g a m e = 
    let n = newSearchNode Decision
        h = horizon a
        sampler _ (_,_,_,mcts,g1) = sample g a m e mcts h
        (_,_,_,mcts, g2) = foldr sampler (m,a,0,n,g) [0..simulationsA a]
        (best_action, g3) = genRandomAction g2 e
        actSelector :: (RandomGen g1) => Action -> 
                       (Action,Reward,g1) -> (Action,Reward,g1)
        actSelector act (best_action,best_mean,gen) = 
          case lookup act (children mcts) of
            Nothing -> (best_action,best_mean,gen)
            Just SearchNode{rewardEstimate=expect} ->
              let (tie,g1) = randomR (-1.0,1.0) gen
                  mean = expect + tie*0.0001
              in if mean > best_mean then (act,mean,g1) 
                 else (best_action,best_mean,g1)
        (action,_,g4) = foldr actSelector (best_action,-1,g3) [0..maxAction e]
    in (action,g4)
             
data AgentA = Agent {
  ageA :: Int,
  totalRewardA :: Reward,
  horizonA :: Int,
  learningPeriodA :: Int,
  simulationsA :: Int
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Monte Carlo Tree Search

data NodeType = Chance | Decision

data SearchNode = SearchNode {
  rewardEstimate :: Double,
  visits :: Int,
  nodetype :: NodeType,
  children :: Map Interaction SearchNode
  }
  
-- Exploration constant for UCB action policy
explorationConstant :: Double
explorationConstant = 2.0

newSearchNode :: NodeType -> SearchNode
newSearchNode t = SearchNode {
  rewardEstimate = 0,
  visits = 0,
  nodetype = t,
  children = empty
  }
                  
-- Select an action according to UCB policy
selectAction :: (Model m, Environment e, Agent a, RandomGen g) =>
                g -> a -> m -> e -> SearchNode ->  (Action,g)
selectAction g a m e n =
  let explore_bias = (fromIntegral.horizon) a * (fromIntegral.maxReward) e
      unexplored_bias = fromIntegral (maxBound::Int)
      log_visits = (log.fromIntegral.visits) n
      tlist = map (\x -> (x,lookup x (children n))) [0..maxReward e]
      tiebreak :: (RandomGen g) => g -> (Double, Action) -> (Double, Action) 
                  -> (Double, Action, g)
      tiebreak g (best_priority, best_action) (priority, action) = 
        let (tie, g1) = randomR (-1.0, 1.0) g
        in if (priority > best_priority + tie*0.001)
           then (priority, action, g1)
           else (best_priority, best_action, g1)
                
      traverser :: (RandomGen g) => (Action, Maybe SearchNode) -> 
                   (Double,Action,g) -> (Double,Action,g)
      traverser (action, node) (best_priority, best_action, g) = 
        tiebreak g (priority, action) (best_priority, best_action)      
        where priority = case node of
                Nothing -> unexplored_bias
                Just SearchNode{visits=0} -> unexplored_bias
                Just SearchNode{visits=v, rewardEstimate=r} ->
                  r + explore_bias * sqrt(explorationConstant * log_visits / 
                                          fromIntegral v)
      (p,action,g1) = foldr traverser (fromIntegral (minBound::Int),0,g) tlist
  in (action,g1)
  

sample :: (Model m, Environment e, Agent a, RandomGen g) =>
          g -> a -> m -> e -> SearchNode -> Int -> (m,a,Reward,SearchNode,g)
-- Samples from a monte carlo tree        
-- returns the updated tree, agent, model, reward and generator
updateReward :: Reward -> SearchNode -> SearchNode
updateReward r n@(SearchNode{visits=v, rewardEstimate = mean}) =
  n{visits = v+1,
    rewardEstimate = r+(fromIntegral v)*mean / (fromIntegral v + 1)}

sample g a m e n 0 = (m,a,0.0,n,g)
sample g a m e n@(SearchNode{nodetype=Chance}) h =
  let 
    ((m1,a1),(o,r),g1) = genPerceptAndUpdate g (m,a) e 
    child1 = findWithDefault (newSearchNode Decision) o (children n)
    (m2,a2,r2,n2,g2) = sample g1 a1 m1 e child1 (pred h)
    reward = r2 + fromIntegral r
  in
   (m2,a2,reward,updateReward reward n{children = insert o n2 (children n)}, g2)
   
sample g a m e n@(SearchNode{visits=0}) h = 
  let ((m1,a1),r1,g1) = playout g (m,a) e h
  in (m1, a1, r1, updateReward r1 n, g1)

sample g a m e n h = 
  let (act, g1) = selectAction g a m e n
      (m1,a1) = modelUpdate2 (m,a) e act
      child1 = findWithDefault (newSearchNode Chance) act (children n)
      (m2,a2,r2,n2,g2) = sample g1 a1 m1 e child1 (pred h)
  in (m2,a2,r2,updateReward r2 n{children = insert act n2 (children n)}, g2)