module Util.Util where

-- |'fib' returns given Fibonacci number
fib :: Int -> Int
fib = (!!) fibs

-- |'fibs' is a list of Fibonacci numbers
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
