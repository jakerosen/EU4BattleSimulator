module Main where

import Data.Foldable

main :: IO ()
main = do
  formatBattleLines

formatBattleLines :: IO ()
formatBattleLines = do
  for_ [1..40] (\_ -> putStr "O")
  putStrLn ""
  putStr "C"
  for_ [1..38] (\_ -> putStr "X")
  putStrLn "C"

  putStrLn ""

  putStr "C"
  for_ [1..38] (\_ -> putStr "X")
  putStrLn "C"
  for_ [1..40] (\_ -> putStr "O")
  putStrLn ""
