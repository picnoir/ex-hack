module Cli (
  step,
  promptUser
) where

import Control.Monad (when)

import Log (logTitle)

step :: String -> IO Bool -> IO () -> IO ()
step n pc a = do
  logTitle n
  preCond <- pc
  when preCond a

promptUser :: String -> IO Bool
promptUser str = do
  putStrLn (str ++ " [y/N]")
  res <- getLine
  if head (words res) == "y"
    then return True
    else return False
