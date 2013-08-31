{-# LANGUAGE ExistentialQuantification #-}
module Model where
import Util
import System.Random

class Model m where 
  update :: Bool -> m -> m
  predict :: m -> Bool -> Double
  makeNewModel :: Options -> m
  historySize :: m -> Int
  updateHistory :: m -> [Bool] -> m
  getHistory :: m -> [Bool]

genRandom :: (Model m) => m -> StdGen -> (Bool,StdGen)
genRandom x g = let p = predict x True
                    (k, g1) = randomR (0.0, 1.0) g
                in (k < p, g1)

updateList :: (Model m) => [Bool] -> m -> m  
updateList [] tree = tree
updateList (b:bs) tree = 
  updateList bs newtree
    where newtree = update b tree 

genRandomList :: (Model m) => m -> StdGen -> Int -> ([Bool],StdGen)
genRandomList x g n = 
    let (l,g1,new) = genRandomSymbols x g n                             
    in (l,g1)

genRandomSymbols::(Model m) => m -> StdGen -> Int -> ([Bool],StdGen,m)
genRandomSymbols x g 0 = ([],g,x)
genRandomSymbols x g n = 
  let (b,g1) = genRandom x g
  in genRandomSymbols (update b x) g1 (n-1)
     
data ModelP = forall a. Model a => ModelP a

instance Model ModelP where
  update b (ModelP mode) = ModelP (update b mode)
  predict (ModelP mode) = predict mode
  makeNewModel opts = error "ModelP is an abstract type"
  updateHistory (ModelP mode) b = ModelP (updateHistory mode b)
  historySize (ModelP mode) = historySize mode
  getHistory (ModelP mode) = getHistory mode