module BattleSimulator.Battle where

import BattleSimulator.Player
import BattleSimulator.Unit
import BattleSimulator.Phase

-- This function will need to open up the battle lines and have each unit
-- attempt to find a target.  After targets have been found, that player can
-- inflict casualties upon the other player.  Each player can be treated
-- separately where they individually find targets and calculate how many
-- casualties they inflict on the other player, each returning the new other
-- player as the result of their attack.
engageBattleDay :: Phase -> Player -> Player -> (Player, Player)
engageBattleDay = undefined

-- Since there are some pretty random modifiers that can occur, like some
-- nations having extra fire damage or reduced fire damage taken, or units
-- having extra fire or shock damage from army drilling and that sort of thing,
-- or even steltsy special units, it might make sense to have a list of 'extra'
-- modifiers to apply.
inflictCasualties
  :: Int              -- Dice roll result of attacker
  -> Unit             -- Attacking unit
  -> Unit             -- Defending unit
  -> Unit             -- Damaged defending unit
inflictCasualties dieRoll atkUnit defUnit = undefined

rollDice
  :: Int              -- Dice roll of phase for attacker
  -> Int              -- Terrain modifier
  -> Int              -- Attacking unit's general's pips for this phase
  -> Int              -- Defending unit's general's pips for this phase
  -> Int              -- Attacking unit's pips for this phase
  -> Int              -- Defending unit's pips for this phase
  -> Int              -- Dice roll result
rollDice roll tmod atkGen defGen atkUnit defUnit
  = roll + tmod + atkGen - defGen + atkUnit - defUnit
