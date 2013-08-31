module Util where
import Data.Map as M 
import Data.BitVector
import Data.List
import Data.Maybe
import Control.Monad.Reader
import System.Random

--We first describe a series of types that are useful
-----------------------------------------------------

-- Describes the reward accumulated by an agent
type Reward = Double

{- Describes an interaction (observation, reward or action)
between the agent and the environment
Note that we use Int to encode a vector of bits -}

type Interaction = Integer

-- Describe an action or percept
type Observation = [Bool]

type Percept = Interaction
type Action = Interaction


-- Describes the age of an agent

type Age = Integer


-- The progam's keyword/value option pairs.

type Options = Map String String


---------------------------------------
--We now describe the type classes used.


-- The Model Class; used for learning and prediction.



-- The Environment our agent will interact with


-- Search nodes for Monte Carlo Tree Search
      








----------------------------------------
--We now implement various helper functions


--Gets an option or exists with an error.

getRequiredOption :: String -> Options -> String
getRequiredOption s opts = 
 if member s opts
 then fromJust $ M.lookup s opts  
 else notfound s

notfound s = error $ 
          "ERROR: Option '" ++ s ++           
          "' is required, but it was not present."
           



-- The State of the World
