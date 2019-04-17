module BattleSimulator.Player where

import Data.Map (Map)
import qualified Data.Map as Map
import BattleSimulator.Node

data Player
  = Player
  { role :: Role
  , line :: Map Location Node
  }

data Role = Attacker | Defender;

-- front or back line
data Side = Front | Back

-- location on a battlefield, front or back line with id number of node
type Location = (Side, Int)
