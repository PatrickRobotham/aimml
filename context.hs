-- Context Tree Code
import System.Random

data CTNode = CTNode { zeroes :: Int
                     , ones  :: Int 
                     , kt :: Rational
                     } deriving (Show)
                                                              
data CTTree = CTTree CTNode CTTree CTTree | Empty deriving (Show)

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

update :: CTNode -> Bool -> CTNode
update x b = CTNode {zeroes = zeroes x + b2int (not b), 
                     ones = ones x + b2int b,
                     kt = kt x * ktmultiply x b}

wprob :: CTTree -> Rational
wprob Empty = error "Context Trees are complete binary trees of depth > 0"      
wprob (CTTree x Empty Empty) = kt x
wprob (CTTree x l r) = kt x / 2  + wprob l * wprob r / 2



updateTree :: CTTree -> [Bool] -> Bool -> CTTree
-- updates a context tree based on a history.
updateTree _ [] _ = error "Not enough context!"
updateTree a@(CTTree x l r) (b:bs) bit
  | leafnode a = CTTree updated l r
  | b == True  = CTTree updated (updateTree l bs bit) r
  | b == False = CTTree updated l (updateTree r bs bit)
  where updated = update x bit 

updateTreeBits :: CTTree -> [Bool] -> [Bool] -> CTTree
updateTreeBits tree _ [] = tree
updateTreeBits tree hist (b:bs) = updateTreeBits newtree (b:hist) bs
                                  where newtree = updateTree tree hist b

depth :: CTTree -> Int
depth Empty = 0
depth (CTTree _ l r) = 1 + depth l

predict :: CTTree -> [Bool] -> Bool ->  Rational
-- The conditional probability of a bit given the history
predict x hist b 
  | length hist < depth x - 1 = 1/2
  | otherwise = wprob (updateTree x hist b)/ wprob x

predictBlock :: CTTree -> [Bool] -> [Bool] -> Rational
-- The conditional probability of a list of bits given the history
predictBlock x hist bits 
  | length hist < depth x - 1 = (1/2)^(length bits)
  | otherwise = wprob (updateTreeBits x hist bits) / wprob x
                

genRandomBit :: (RandomGen g) => CTTree -> [Bool] -> g -> (Bool,g)
-- Generates a random bit with probability taken from CTTree
genRandomBit x hist g =
  let p = fromRational (predict x hist True)
      (k,g1) = randomR (0 :: Double, 1) g
  in 
   (k < p, g1)

genRandomBlock :: (RandomGen g) => CTTree -> [Bool] -> g -> Int -> ([Bool],g)
-- Generates a list of random bits with specified length
genRandomBlock x hist g 0 = ([],g)
genRandomBlock x hist g n = 
  let (b,g1) = genRandomBit x hist g 
  in genRandomBlock (updateTree x hist b) (b:hist) g1 (n-1)

makeNewContextTree :: Int -> CTTree
-- Creates a context tree of specified depth
makeNewContextTree 0 = Empty
makeNewContextTree n 
  | n < 0 = error "depth must be positive"
  |otherwise = CTTree newnode newchild newchild
    where
      newnode = CTNode {zeroes = 0, ones = 0, kt = 0.5}
      newchild = makeNewContextTree $ n-1