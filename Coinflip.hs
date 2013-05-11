module Coinflip (CoinFlip,
 performAction,
 maxAction,
 maxObservation,
 maxReward,
 logger)
       
where
import Environment
import System.Random

data Coin = Head | Tail

data CoinFlip = Flipper{
  coin :: Coin,
  seed :: StdGen}
  
