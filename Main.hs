module Main where

import Data.Foldable
import BattleSimulator.Player

main :: IO ()
main = do
  return ()

player1 :: Player
player1 =
  Player
    Attacker
    40
    0
    0
    0
    0
    initialLine

player2 :: Player
player2 =
  Player
    Defender
    40
    0
    0
    0
    0
    initialLine

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
