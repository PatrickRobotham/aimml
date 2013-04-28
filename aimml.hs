-- 

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
