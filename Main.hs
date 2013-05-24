{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Main where
import Typedefs
import Agent
import Environment
import Predict
import qualified Data.Map as M
import Text.Regex
import Data.Char
import System.Random
import Data.Bits
import Control.Monad
import System.Environment
import Util
import Coinflip

processOptions :: String -> Options
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
      . map (filter (not.isSpace)) . map stripComments . lines) str
      
                               
mainloop :: (Agent a, Environment e, Model m) => a -> m -> e -> 
            Options -> IO ()
mainloop ai model env opts = do
  -- Get options
  seed  <- (return.read) $ M.findWithDefault "0" "random-seed" opts :: IO Int
  print seed
  verbose <- return $ M.findWithDefault "False" "verbose" opts == "true" :: IO Bool
  -- There's some exploration options which I ignore
  aiage <- (return.read) $ M.findWithDefault "0" "terminate-age"  opts:: IO Int
  learningPeriod <- (return.read) $ 
                    M.findWithDefault "0" "learning-period" opts :: IO Int 
  print "test"
  interactionLoop (mkStdGen seed) ai model env 1 verbose aiage

powerof2 :: Int -> Bool
-- Checks whether x == 2^n for some n
powerof2 x = x .&. (x-1) == 0

interactionLoop g a m e counter verbose aiage  =
  if isFinished e || age a > aiage
  then finish a 
  else do
    -- Get a percept from the environment
    o <- return (getObservation e)  
    r <- return $ getReward e
    -- Update agent's environment model with the new percept   
    (m,a) <- return $ modelUpdate (m,a) e (o,r)
    -- Determine best action
    (action, g) <- return $ search g a m e
    -- Send action to environment
    e <- return $ performAction action e
    -- Update agent's environment model
    (m,a) <- return $ modelUpdate2 (m,a) e action
    -- Log turn
    -- TODO FILL IN
    -- Print to standard output when counter == 2^n or verbose
    when (verbose || powerof2 counter) $
      do 
        putStrLn $ "cycle: " ++ show counter
        putStrLn $ "average reward: " ++ show (averageReward a)
        return ()
    when verbose $ print e
    -- Update explore rate
    -- TODO Fill in
    interactionLoop g a m e (succ counter) verbose aiage
    
finish :: (Agent a) => a -> IO ()
finish a = 
  do
    putStrLn "\n \n SUMMARY"
    putStrLn $ "agent age: " ++ (show.age) a 
    putStrLn $ "average reward: " ++ (show . averageReward) a
    
environmentReader :: Options -> CoinFlip
environmentReader o 
  | getRequiredOption "environment" o  == "coin-flip" = makeNewEnvironment o  
--  where name = Util.getRequiredOption "environment" o

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
  defaults <- return $  
              M.fromList [("ct-depth","30"),
                          ("agent-horizon","5"),
                          ("exploration","0.0"),
                          ("explore-decay","1.0"),
                          ("mc-simulations","300")]
  
  -- read configuration options
  conf <- readFile $ head args
  options <- return $ processOptions conf
  options <- return $ M.union options defaults 
  -- Set up environment
  e <- return $ environmentReader options 
  -- Set up model
  m <- return $ makeNewModel options :: IO ContextTree
  -- Set up agent
  a <- return $ makeAgent options :: IO AgentA
  -- Run main agent/environment interaction loop
  mainloop a m e options
  -- TODO deal with logger (it's the second argument)  

    