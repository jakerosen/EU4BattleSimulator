module BattleSimulator.Node where

import BattleSimulator.Unit

data Node =
  Node
  { idn :: Int -- maps to a node in front or back
  , unit :: Maybe Unit -- Nothing if unit there is no unit in this spot
  }
