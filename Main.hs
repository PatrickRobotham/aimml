{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -XScopedTypeVariables-}
module Main where
import Typedefs
import Agent
import Environment
import Predict
import qualified Data.Map as M
import Text.Regex
import Data.Char

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
            Options -> IO a
mainloop ai model env opts = do
  -- Get options
  seed  <- (return.read) $ M.findWithDefault "random-seed" "0" opts :: IO Int
  verbose <- (return.read) $ M.findWithDefault "verbose" "False" opts :: IO Bool
  -- There's some exploration options which I ignore
  age <- (return.read) $ M.findWithDefault "terminate-age" "0"  opts:: IO Age
  learningPeriod <- (return.read) $ 
                    M.findWithDefault "learning-period" "0" opts :: IO Int 
  interactionLoop 
  
