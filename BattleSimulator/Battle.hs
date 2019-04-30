module BattleSimulator.Battle where
import Data.List
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
      inflictedCasualties1 :: Vector (Int, Double)
      inflictedCasualties1 = inflictedCasualties phase p2 p1

      -- casualties inflicted from player1 on line2
      inflictedCasualties2 :: Vector (Int, Double)
      inflictedCasualties2 = inflictedCasualties phase p1 p2

      applyCasualties
        :: Map Position (Maybe Unit)
        -> Int
        -> (Int, Double)
        -> Map Position (Maybe Unit)
      applyCasualties line i casualties =
        Map.adjust (fmap (takeCasualties casualties)) (Front, i) line


      endOfDayLine1 :: Map Position (Maybe Unit)
      endOfDayLine1 =
        -- I'm going to have to think about how this function works.  It's
        -- line one that would find the targets to deal damage to line2, but
        -- I can't map over line1 to get the casualty results for line2, since
        -- it would be inherently placing the results into line1.  That
        -- doesn't make sense.  I probably need my own function for doing
        -- this thing.

        -- apply inflictedCasualties1 to line1
        Vector.ifoldl'
          applyCasualties -- Map of line -> (casualties, morale) -> result Map
          line1           -- default value for fold
          inflictedCasualties1


      endOfDayLine2 :: Map Position (Maybe Unit)
      endOfDayLine2 =
        Vector.ifoldl'
          applyCasualties -- Map of line -> (casualties, morale) -> result Map
          line2           -- default value for fold
          inflictedCasualties2

      endOfDayPlayer1 :: Player
      endOfDayPlayer1 =
        Player role1 diceRoll1 tmod1 fire1 shock1 endOfDayLine1

      endOfDayPlayer2 :: Player
      endOfDayPlayer2 =
        Player role2 diceRoll2 tmod2 fire2 shock2 endOfDayLine2

-- Since there are some pretty random modifiers that can occur, like some
-- nations having extra fire damage or reduced fire damage taken, or units
-- having extra fire or shock damage from army drilling and that sort of thing,
-- or even steltsy special units, it might make sense to have a list of 'extra'
-- modifiers to apply.
inflictCasualties
  :: Phase            -- Which mod am I getting
  -> Int              -- Dice roll of phase attacker
  -> Int              -- Dice roll of morale attacker
  -> Position         -- position of attacker (for checking backline, etc.)
  -> Unit             -- Attacking unit
  -> Unit             -- Defending unit
  -> [Double]         -- list of 'extra' modifiers (possibility)
  -> (Int, Double)       -- return (casualties, morale) inflicted
inflictCasualties
  phase
  dieRollPhase
  dieRollMorale
  atkPos
  atkUnit
  defUnit
  extraMods =
    let
      mod :: Double
      mod = case atkPos of
        (Back, _) -> case unitType atkUnit of
          Artillery -> 0.5
          _ -> 0
        (Front, _) -> 1

      baseCasualties :: Int
      baseCasualties = 15 + 5 * dieRollPhase

      baseMoraleCasualties :: Int
      baseMoraleCasualties = 15 + 5 * dieRollMorale

      atkMaxMorale :: Double
      atkMaxMorale = maxMorale atkUnit

      atkStrength :: Double
      atkStrength = (fromIntegral $ strength atkUnit) / 1000

      atkMod :: Double
      atkMod = phaseMod phase atkUnit

      atkCombatAbility :: Double
      atkCombatAbility = combatAbility atkUnit

      atkDiscipline :: Double
      atkDiscipline = discipline atkUnit

      defTactics :: Double
      defTactics = tactics defUnit * (discipline defUnit)

      casualtiesIntermediate :: Double
      casualtiesIntermediate
        = atkStrength
        * atkMod
        * atkCombatAbility
        * atkDiscipline
        * defTactics
        * foldl' (*) 1 extraMods

      casualties :: Int
      casualties = floor
        $ (fromIntegral baseCasualties)
        * casualtiesIntermediate

      moraleCasualties :: Double
      moraleCasualties =
          (fromIntegral baseMoraleCasualties)
        * casualtiesIntermediate
        * atkMaxMorale
        / 600

    in
      (casualties, moraleCasualties)


-- this is the (casualties, morale) that the first player inflicts
-- on the second
-- the index of the vector is the target taking the casualties
inflictedCasualties :: Phase -> Player -> Player -> Vector (Int, Double)
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

    diceRollIntermediate :: Int
    diceRollIntermediate =
      diceRoll1 + tmod1 + phaseSkill1 - phaseSkill2


    -- plan:
    -- map over 0-39 front and back for line1 (which is a Map),
    -- for each unit in that map (i.e. for each unit on that side's
    -- field), attempt to find a target.  If found return
    -- Just (ID# of target, (casualties, Morale)) for casualties
    -- inflicted to the target.  Apply, using Vector.(//) (bulk update),
    -- to a vector that represents casualty damage.  Return that
    -- resulting Vector.
    casualtiesList :: [(Int, (Int, Double))]
    casualtiesList = catMaybes
      $  (attemptInflict <$> (zip (repeat Front) [0,39]))
      ++ (attemptInflict <$> (zip (repeat Back) [0,39]))

      where
        -- This function needs to have the unit that corresponds to
        -- this index attempt to find a valid target to inflict
        -- casualties to.  If found, inflict those casualties to that
        -- unit.
        attemptInflict :: Position -> Maybe (Int, (Int, Double))
        attemptInflict pos@(l, i) = do
          unitAttacker :: Unit <- line1 ! pos

          let
            findTarget :: Maybe Int -- target ID#, if found
            findTarget =
              -- strategy:
              -- find list of valid indecies starting from i, going towards
              -- the middle flanking -1 times.
              -- use this list to find first Just value, if it exists
              find (\x -> isJust (line2 ! (Front, x))) indecies

              where
                step :: Int
                step = if i < 39 `div` 2 then 1 else -1

                indecies :: [Int]
                indecies =
                  [  i        -- start across
                  ,  i + step -- step towards center
                  .. i + step * ((flanking unitAttacker) - 1)]
                              -- flanking -1 number of spaces to look for unit

          target :: Int <- findTarget
          unitTarget :: Unit <- line2 ! (Front, target)

          let
            supportUnit :: Maybe Unit
            supportUnit = line2 ! (Back, target)

            supportPhaseBonus :: Int
            supportPhaseBonus = case supportUnit of
              Nothing -> 0
              Just sup -> case unitType sup of
                Artillery -> case phase of
                  Fire  -> dFirePips sup `div` 2
                  Shock -> dFirePips sup `div` 2
                _ -> 0

            supportMoraleBonus :: Int
            supportMoraleBonus = case supportUnit of
              Nothing -> 0
              Just sup -> case unitType sup of
                Artillery -> dMoralePips sup `div` 2
                _ -> 0

            --       offensive phase pips of attacking unit
            -- minus defensive phase pips of defending unit
            deltaUnitPhasePips :: Int
            deltaUnitPhasePips = case phase of
              Fire ->
                  (oFirePips unitAttacker)
                - (dFirePips unitTarget)
              Shock ->
                  (oShockPips unitAttacker)
                - (dShockPips unitTarget)

            diceRollPhase :: Int
            diceRollPhase =
                diceRollIntermediate
              + deltaUnitPhasePips
              - supportPhaseBonus

            diceRollMorale :: Int
            diceRollMorale =
                diceRollIntermediate
              + (oMoralePips unitAttacker)
              - (dMoralePips unitTarget)
              - supportMoraleBonus

            -- extra modifiers that a unit or player or whatever might have;
            -- this is not really implemented at all yet, so I still have to
            -- consider how I am going to approach that in the future.
            extraMods :: [Double]
            extraMods = []

          Just
            (  target
            , (inflictCasualties
                 phase
                 diceRollPhase
                 diceRollMorale
                 pos
                 unitAttacker
                 unitTarget
                 extraMods) )
