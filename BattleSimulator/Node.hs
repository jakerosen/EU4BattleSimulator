module BattleSimulator.Node where

data Node =
  Node
  { idn :: Int -- maps to a node in front or back
  --, unit :: Maybe Unit
  }
