{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Main where

import Search
import Util
import Environment
import Coinflip
import Pacman
import ContextTree
import Model
import qualified Data.Map as M
import Text.Regex
import Data.Char
import System.Random
import Data.Bits
import Control.Monad
import System.Environment
import Util
import Control.Monad.State
import World
import System.Exit
import qualified Data.HashTable.IO as H

type HashTable k v = H.LinearHashTable k v




processOptions :: String -> Options
-- Reads a configuration file
processOptions str =
  let splitEq :: String -> (String,String)                         
      splitEq str = if length splits == 2    
                    then (splits !! 0, splits !! 1)
                    else error "Must have exactly one '=' per line."
                      where      
                        splits = splitRegex (mkRegex "=") str
      ignore :: String -> Bool
      ignore str = head str == '#' || length str == 0
      stripComments :: String -> String
      stripComments str = head $ splitRegex (mkRegex "#") str
  in (M.fromList . map splitEq . filter (not.ignore)
      . map (filter (not.isSpace)) . map stripComments . 
      filter (not.ignore) . lines) str
     

powerof2 :: Int -> Bool
-- Checks whether x == 2^n for some n
powerof2 x = x .&. (x-1) == 0

environmentReader :: Options -> EnvironmentP
environmentReader o 
  | getRequiredOption "environment" o  == "coin-flip" =  
    EnvironmentP (makeNewEnvironment o :: CoinFlip)
  | getRequiredOption "environment" o == "pacman" =
      EnvironmentP (makeNewEnvironment o :: Pacman)
modelMaker :: Options -> ModelP
-- TODO: Add option for Hoeffding Trees
modelMaker o = ModelP(makeNewModel o :: ContextTree)

interactionLoop counter verbose aiage = do
  e <- gets env
  a <- gets agent
 
  -- Print Environment
  liftIO $ print e
  -- Determine best action
  act <- search
  actE <- encodeAction act
  -- Perform Action
  modifyEnvironment (performAction act)
  -- Update agent's environment model
  updateModelAction act
  -- Get a percept from the environment
  o <- gets (getObservation.env) 
  r <- gets (getReward.env)
  or <- encodePercept (o,r)
  -- Update agent with new percept
  updateModelPercept (o,r)
  a <- gets agent
  let a' = a{age = (age a) + 1,
             totalReward = (totalReward a) + fromIntegral r}
  modify (\x -> x{agent = a'})
  -- print to standard output when counter == 2^n or verbose
  when (verbose || powerof2 counter) $
    do 
      a <- gets agent
      liftIO $ do 
        putStrLn $ "cycle:" ++ show counter;
        putStrLn $ "average reward: " ++ show (averageReward a)
  when (not $ isFinished e || age a > aiage) $
    interactionLoop (succ counter) verbose aiage
  finish
  
finish :: World ()
finish = do
  a <- gets agent
  e <- gets env
  liftIO $ putStrLn "\n \n SUMMARY"
  liftIO $ putStrLn $ "agent age: " ++ (show.age) a
  liftIO $ putStrLn $ "average reward:" ++ (show . averageReward) a
main :: IO ()
main = do
  args <- getArgs
  when (length args > 2 || null args)
    (error $   
     "Incorrect number of arguments \n" ++
     "The first argument should indicate the location of the" ++
     "configuration file and the second (optional) argument" ++ 
     " should indicate the file to log to"
    )      
  -- Default configuration values
  let defaults =  
        M.fromList [("ct-depth","30"),
                    ("agent-horizon","5"),
                    ("exploration","0.0"),
                    ("explore-decay","1.0"),
                    ("mc-simulations","300")]
  -- read configuration options
  conf <- readFile $ head args
  let options = M.union (processOptions conf) defaults
  -- Set up environment
  let e = environmentReader options
  -- Set up agent
  let a = makeAgent options
  -- Set up model
  let m = modelMaker options
  g <- getStdGen
  verbose <- return $ M.findWithDefault "False" "verbose" options == "true"
  -- There's some exploration options which I ignore
  aiage <- (return.read) $ M.findWithDefault "0" "terminate-age" options :: IO Int
  -- Set up state of world
  tr <- H.new
  let world = WorldState{
        env = e,
        agent = a, 
        model = m,
        tree = tr,
        gen = g,
        history = [],
        reward = 0,
        sampleReward = 0
        }
  -- Run interaction loop
  worldEnd <- execStateT (interactionLoop 1 verbose aiage) world
  -- Finish
  return ()
