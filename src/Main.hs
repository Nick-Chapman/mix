module Main where

import Prelude hiding (gcd)

import Flow (gcd,run,Value(N))

main :: IO ()
main = do
  putStrLn "*mix*"
  print gcd
  print $ check (run gcd [N 1386, N 3213]) (N 63)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
