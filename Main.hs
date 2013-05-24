{-# LANGUAGE ScopedTypeVariables #-}
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
  in (M.fromAscList . map splitEq . filter (not.ignore)
      . map (filter (not.isSpace)) . lines) str
      
                               
mainloop :: (Agent a, Environment e, Model m) => a -> m -> e -> 
            Options -> IO ()
mainloop ai model env opts = do
  -- Get options
  seed  <- (return.read) $ M.findWithDefault "random-seed" "0" opts :: IO Int
  verbose <- (return.read) $ M.findWithDefault "verbose" "False" opts :: IO Bool
  -- There's some exploration options which I ignore
  aiage <- (return.read) $ M.findWithDefault "terminate-age" "0"  opts:: IO Int
  learningPeriod <- (return.read) $ 
                    M.findWithDefault "learning-period" "0" opts :: IO Int 
  interactionLoop (mkStdGen seed) ai model env 1 verbose aiage

powerof2 :: Int -> Bool
-- Checks whether x == 2^n for some n
powerof2 x = x .&. (x-1) == 0

interactionLoop g a m e counter verbose aiage  =
  if isFinished e || age a > aiage
  then finish a
  else  do  
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
    
main :: IO ()
main = print "Fill this in Patrick!"