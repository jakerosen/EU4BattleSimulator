module BattleSimulator.Player where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
-- import BattleSimulator.Node -- candidate to be removed
import BattleSimulator.Unit

-- note: I'm punting on additional fire and shock modifiers for now.
-- They might go here, or might go elsewhere.
data Player
  = Player
  { role :: Role
  , phaseDiceRoll :: Int
  , terrainMod :: Int
  , generalFire :: Int
  , generalShock :: Int
  , line :: Map Location (Maybe Unit)
  }

data Role = Attacker | Defender;

-- front or back line
data Line = Front | Back

-- location on a battlefield, front or back line with id number of node
-- I'm considering having the numbering start with 0 in the middle,
-- positive counting up towards the right, and negative counting down towards
-- the left.  Consequently, one side (Attacker or Defender) will have to be
-- reversed so that the nodes line up when they face each other.  I believe
-- that this will be the most convenient setup for finding targets.
type Location = (Line, Int)
