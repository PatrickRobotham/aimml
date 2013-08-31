import System.Random
import Control.Monad.State



data WorldState = WorldState {
  env :: e, 
  model :: m,
  tree :: SearchNode, -- Replace with pointer?
  gen :: StdGen,
  agent :: Agent
  }

type WorldStateT = StateT WorldState