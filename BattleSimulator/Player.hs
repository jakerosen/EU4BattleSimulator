module BattleSimulator.Player where

import Data.Vector
import BattleSimulator.Node

data Player
  = Player
  { front :: Vector Node
  , back :: Vector Node
  }
