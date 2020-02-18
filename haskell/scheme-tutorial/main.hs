-- main.hs
-- Haskell tutorial from here:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)
