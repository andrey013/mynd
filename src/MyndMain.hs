module Main where

import Util.Util

-- |'main' runs the main program
main :: IO ()
main = do
  putStrLn . show $ fib 100