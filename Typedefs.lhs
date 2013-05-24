> module Typedefs where
> import Data.Map
> import Data.BitVector
> import Data.List
> type Symbol = Bool

The symbols that can be predicted

> type SymbolList = [Symbol]

A list of Symbols

> type Reward = Double

Describes the reward accumulated by an agent

> type Interaction = Integer

Describes an interaction (observation, reward or action)
between the agent and the environment
Note that we use Int to encode a vector of bits

> type Percept = Interaction
> type Action = Interaction

Describe an action or percept (observation or reward)

> type Age = Integer

Describes the age of an agent

> type Options = Map String String

The progam's keyword/value option pairs.

> data AgentOptions = AgentOptions {horizonO :: Int, 
>  simulationsO :: Int, learningPeriodO :: Int, modelOptions :: ModelOptions}

> data ModelOptions = ModelOptions {size :: Int}

> argmax :: (Ord b) => (a->b) -> [a] -> a
> argmax f lst = (fst . maximumBy (\ x y -> compare (snd x) (snd y))) 
>  $ zip lst (Prelude.map f lst)
