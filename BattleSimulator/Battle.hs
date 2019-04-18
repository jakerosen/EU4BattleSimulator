module BattleSimulator.Battle where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
engageBattleDay
  phase
  (Player role1 diceRoll1 tmod1 fire1 shock1 line1)
  (Player role2 diceRoll2 tmod2 fire2 shock2 line2)
  = (endOfDayPlayer1, endOfDayPlayer2)

    where
      phaseSkill1 :: Int
      phaseSkill1 = case phase of
        Fire -> fire1
        Shock -> shock1

      phaseSkill2 :: Int
      phaseSkill2 = case phase of
        Fire -> fire2
        Shock -> shock2

      -- this is the casualties that line1 inflicts on line2
      -- ( ID number of unit inflicted upon
      -- , casualties
      -- , morale damage )
      inflictedCasualties1 :: [(Int, Int, Int)]
      inflictedCasualties1 = undefined

      endOfDayLine2 :: Map Location (Maybe Unit)
      endOfDayLine2
        -- I'm going to have to think about how this function works.  It's
        -- line one that would find the targets to deal damage to line2, but
        -- I can't map over line1 to get the casualty results for line2, since
        -- it would be inherently placing the results into line1.  That
        -- doesn't make sense.  I probably need my own function for doing
        -- this thing.
        -- = Map.mapWithKey _ line1
        = undefined

      endOfDayLine1 :: Map Location (Maybe Unit)
      endOfDayLine1
        = undefined

      endOfDayPlayer1 :: Player
      endOfDayPlayer1
        = Player role2 diceRoll2 tmod2 fire2 shock2 endOfDayLine2

      endOfDayPlayer2 :: Player
      endOfDayPlayer2
        = Player role2 diceRoll2 tmod2 fire2 shock2 endOfDayLine2

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
