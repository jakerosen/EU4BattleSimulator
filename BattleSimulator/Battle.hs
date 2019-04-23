module BattleSimulator.Battle where
import Data.Maybe
import Data.Map (Map, (!))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
  p1@(Player role1 diceRoll1 tmod1 fire1 shock1 line1)
  p2@(Player role2 diceRoll2 tmod2 fire2 shock2 line2)
  = (endOfDayPlayer1, endOfDayPlayer2)

    where
      -- casualties inflicted from player2 on line1
      inflictedCasualties1 :: Vector (Int, Int)
      inflictedCasualties1 = inflictedCasualties phase p2 p1

      -- casualties inflicted from player1 on line2
      inflictedCasualties2 :: Vector (Int, Int)
      inflictedCasualties2 = inflictedCasualties phase p1 p2

      applyCasualties
        :: Map Position (Maybe Unit)
        -> Int
        -> (Int, Int)
        -> Map Position (Maybe Unit)
      applyCasualties line i (casualties, morale) = undefined


      endOfDayLine1 :: Map Position (Maybe Unit)
      endOfDayLine1
        -- I'm going to have to think about how this function works.  It's
        -- line one that would find the targets to deal damage to line2, but
        -- I can't map over line1 to get the casualty results for line2, since
        -- it would be inherently placing the results into line1.  That
        -- doesn't make sense.  I probably need my own function for doing
        -- this thing.

        -- apply inflictedCasualties1 to line1
        = Vector.ifoldl'
          applyCasualties -- Map of line -> (casualties, morale) -> result Map
          line1           -- default value for fold
          inflictedCasualties1


      endOfDayLine2 :: Map Position (Maybe Unit)
      endOfDayLine2
        = undefined

      endOfDayPlayer1 :: Player
      endOfDayPlayer1
        = Player role1 diceRoll1 tmod1 fire1 shock1 endOfDayLine1

      endOfDayPlayer2 :: Player
      endOfDayPlayer2
        = Player role2 diceRoll2 tmod2 fire2 shock2 endOfDayLine2

-- Since there are some pretty random modifiers that can occur, like some
-- nations having extra fire damage or reduced fire damage taken, or units
-- having extra fire or shock damage from army drilling and that sort of thing,
-- or even steltsy special units, it might make sense to have a list of 'extra'
-- modifiers to apply.
inflictCasualties
  :: Int              -- Dice roll of phase attacker
  -> Int              -- Dice roll of morale attacker
  -> Unit             -- Attacking unit
  -> Unit             -- Defending unit
  -> (Int, Int)       -- return (casualties, morale) inflicted
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

-- this is the (casualties, morale) that the first player inflicts
-- on the second
-- the index of the vector is the target taking the casualties
inflictedCasualties :: Phase -> Player -> Player -> Vector (Int, Int)
inflictedCasualties
  phase
  (Player role1 diceRoll1 tmod1 fire1 shock1 line1)
  (Player role2 diceRoll2 tmod2 fire2 shock2 line2)
  =
    -- accumulate the results of the casualties list into a vector
    -- of casualties
    Vector.accum
      (\(c1, m1) (c2, m2) -> (c1 + c2, m1 + m2)) -- add casualties
                                                 -- and morale
      (Vector.replicate 40 (0,0)) -- initial casualties (0,0)
      casualtiesList
  where
    phaseSkill1 :: Int
    phaseSkill1 = case phase of
      Fire -> fire1
      Shock -> shock1

    phaseSkill2 :: Int
    phaseSkill2 = case phase of
      Fire -> fire2
      Shock -> shock2

    diceRollPhase1 :: Int
    diceRollPhase1 = undefined

    diceRollPhase2 :: Int
    diceRollPhase2 = undefined

    diceRollMorale1 :: Int
    diceRollMorale1 = undefined

    diceRollMorale2 :: Int
    diceRollMorale2 = undefined

    findTarget :: Position -> Maybe Int -- target ID#, if found
    findTarget = undefined

    -- plan:
    -- map over 0-39 front and back for line1 (which is a Map),
    -- for each unit in that map (i.e. for each unit on that side's
    -- field), attempt to find a target.  If found return
    -- Just (ID# of target, (casualties, Morale)) for casualties
    -- inflicted to the target.  Apply, using Vector.(//) (bulk update),
    -- to a vector that represents casualty damage.  Return that
    -- resulting Vector.
    casualtiesList :: [(Int, (Int, Int))]
    casualtiesList = catMaybes
      $  (attemptInflict <$> (zip (repeat Front) [0,39]))
      ++ (attemptInflict <$> (zip (repeat Back) [0,39]))
      where
        -- This function needs to have the unit that corresponds to
        -- this index attempt to find a valid target to inflict
        -- casualties to.  If found, inflict those casualties to that
        -- unit.
        attemptInflict :: Position -> Maybe (Int, (Int, Int))
        attemptInflict pos@(l, i) = do
          target :: Int <- findTarget pos
          unitAttacker :: Unit <- line1 ! pos
          unitTarget :: Unit <- line2 ! (Front, target)
          Just
            (target, (inflictCasualties
              diceRollPhase1
              diceRollMorale1
              unitAttacker
                unitTarget))
