module ContextTree (ContextTree) where
import Model
import Util (getRequiredOption)
--import Data.Dequeue -- Use Dequeues for history <performance>

data CTNode = CTNode { zeroes :: Int
                     , ones  :: Int 
                     , kt :: Rational
                     } deriving (Show)
                                                              
data CTTree = CTTree CTNode CTTree CTTree | Empty deriving (Show)

data ContextTree = ContextTree {history :: [Bool], size :: Int, hlength :: Int,
                                tree :: CTTree} deriving (Show)
                                                         

b2int :: Bool -> Int
b2int False = 0
b2int True = 1

leafnode :: CTTree -> Bool
leafnode (CTTree _ Empty Empty) = True
leafnode _ = False

visits :: CTNode -> Rational
visits x = fromIntegral $ zeroes x + ones x

counts :: CTNode -> Bool -> Int
counts x False = zeroes x
counts x True = ones x

ktmultiply :: CTNode -> Bool -> Rational
ktmultiply x b = (fromIntegral (counts x b) + 1/2) / (visits x + 1)

updateBit :: CTNode -> Bool -> CTNode
updateBit x b = CTNode {zeroes = zeroes x + b2int (not b), 
                     ones = ones x + b2int b,
                     kt = kt x * ktmultiply x b}

wprob :: CTTree -> Rational
wprob Empty = error "Context Trees are complete binary trees of depth > 0"      
wprob (CTTree x Empty Empty) = kt x
wprob (CTTree x l r) = kt x / 2  + wprob l * wprob r / 2

depth :: CTTree -> Int
depth Empty = 0
depth (CTTree _ l r) = 1 + depth l

updateTree :: CTTree -> [Bool] -> Bool -> CTTree
-- updates a context tree based on a history.
updateTree _ [] _ = error "Not enough context!"
updateTree a@(CTTree x l r) (b:bs) bit
  | leafnode a = CTTree updated l r
  | b == True  = CTTree updated (updateTree l bs bit) r -- go left
  | b == False = CTTree updated l (updateTree r bs bit) -- go right
  where updated = updateBit x bit

makeNewContextTree :: Int -> CTTree
-- Creates a context tree of specified depth
makeNewContextTree 0 = Empty
makeNewContextTree n 
  | n < 0 = error "depth must be positive"
  |otherwise = CTTree newnode newchild newchild
    where
      newnode = CTNode {zeroes = 0, ones = 0, kt = 0.5}
      newchild = makeNewContextTree $ n-1


instance Model ContextTree where
  update b c@(ContextTree{history = h, tree = t, hlength = l}) = 
     if hlength c > size c
     then c{history = b:h, tree = updateTree t h b, hlength = l+1}
     else c{history = b:h, hlength = l+1}
  
  makeNewModel o = let size = read $ getRequiredOption "ct-depth" o
                   in ContextTree{history = [], 
                                  tree = makeNewContextTree size, 
                                  size = size, hlength = 0}
                      
  historySize = length.history
  
  getHistory = history
  
  updateHistory c@(ContextTree{history = h}) l = c{history = reverse l ++ h}
  
  predict (ContextTree{history=hist, tree = m}) guess = 
    fromRational $ predict1 m hist guess  
      where
        predict1 :: CTTree -> [Bool] -> Bool ->  Rational
        -- The conditional probability of a bit given the history
        predict1 x hist b 
          | length hist < depth x = 1/2
          | otherwise = wprob (updateTree x hist b)/ wprob x