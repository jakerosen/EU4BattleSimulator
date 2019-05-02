module BattleSimulator.Player where

import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Category ((>>>))
import Control.Lens
import GHC.Generics
import Data.Generics.Product.Any
-- import Control.Monad.State.Lazy
-- import BattleSimulator.Node -- candidate to be removed
import BattleSimulator.Unit

-- note: I'm punting on additional fire and shock modifiers for now.
-- They might go here, or might go elsewhere.
data Player
  = Player
  { role :: Role
  , combatWidth :: Int
  , phaseDiceRoll :: Int
  , terrainMod :: Int
  , generalFire :: Int
  , generalShock :: Int
  , line :: Map Position (Maybe Unit)
  } deriving Generic

data Role = Attacker | Defender

-- front or back line
data Line = Front | Back
  deriving (Eq, Ord)

-- location on a battlefield, front or back line with id number of node
-- I'm considering having the numbering start with 0 in the middle,
-- positive counting up towards the right, and negative counting down towards
-- the left.  Consequently, one side (Attacker or Defender) will have to be
-- reversed so that the nodes line up when they face each other.  I believe
-- that this will be the most convenient setup for finding targets.

-- About the above: I decided against the approach of putting 0 in the middle
-- because it is possible to have an even number of troops, which would throw
-- off the easy calculation of the target across.  Instead, I'll index from
-- 0 on the left and 39 on the right, with no mirroring.  As such, the target
-- across from x can be calculated as target = 39 - x.
type Position = (Line, Int)

-- A battle line initialized with 40 front and 40 back positions, with no units
initialLine :: Map Position (Maybe Unit)
initialLine = Map.fromList list
  where
    list :: [(Position, Maybe Unit)]
    list = zip positions (repeat Nothing)

    positions :: [Position]
    positions = zip (repeat Front) [0..39] ++ (zip (repeat Back) [0..39])

-- Algorithm for forming battle lines:`
-- Smaller line:
--  -> Deploy all infantry in front line, up to combat width - X where X
--     is flanking of cavalry
--  -> Deploy all cavalry adjacent to infantry in front line, up to combat
--     width
--  -> Deploy all artillery in back line, starting from center, up to combat
--     width
--  -> remaining infantry in back line, up to combat width
--  -> remaining cavalry go in back line, starting from edge, going inward
--
-- Larger line:
--  -> Deploy all infantry in front line that can attack enemy units.  In
--     practice, this means that you would deploy all infantry up to the
--     number of units in the opponent's front line + flanking of your
--     infantry
--  -> Deploy all cavalry in front line that can attack enemy units, up to
--     combat width
--  -> Deploy all artillery in back line, up to combat width.
--  -> Deploy all remaining infantry in back line while there are infantry
--     in front, up to combat width
--  -> Deploy all remaining cavalry in back line while there are positions
--     behind cavalry in front, up to combat width
formBattleLines
  :: (Player, [Unit])
  -> (Player, [Unit])
  -> (Player, Player)
formBattleLines group1@(p1, units1) group2@(p2, units2) = undefined
  where
    -- numUnitsLarger :: Int
    -- numUnitsLarger = length _

    -- numUnitsSmaller :: Int
    -- numUnitsSmaller = length _

    (larger, smaller) :: ((Player, [Unit]), (Player, [Unit]))
      = if (length units1) > (length units2)
        then (group1, group2) -- player 1 has a larger line
        else (group2, group1) -- player 2 has a larger line

    widthSmaller :: Int
    widthSmaller = smaller ^. _1 . the @"combatWidth"
    -- widthSmaller = fst smaller & combatWidth
    -- widthSmaller = view (_1 . the @"combatWidth") smaller
    -- widthSmaller = evalState (uses _1 combatWidth) smaller

    widthLarger :: Int
    widthLarger = larger ^. _1 . the @"combatWidth"
    -- fst larger & combatWidth

    -- I can't do the following because I don't believe it would work well if
    -- there aren't all 3 unit types
    -- [smallerInfantry, smallerCavalry, smallerArtillery] :: [[Unit]]
    --   = groupBy (\u1 u2 -> unitType u1 == (unitType u2)) (snd smaller)

    -- partition these units into ([infantry], [cavalry], [artillery])
    partitionUnits :: [Unit] -> ([Unit], [Unit], [Unit])
    partitionUnits units = (infantry, cavalry, artillery)
      where
        (infantry, rest) :: ([Unit], [Unit])
          = partition (unitType >>> (== Infantry)) (snd smaller)

        (cavalry, artillery) :: ([Unit], [Unit])
          = partition (unitType >>> (== Cavalry)) rest

    (smallerInfantry, smallerCavalry, smallerArtillery)
      :: ([Unit], [Unit], [Unit])
      = partitionUnits (snd smaller)

    (largerInfantry, largerCavalry, largerArtillery)
      :: ([Unit], [Unit], [Unit])
      = partitionUnits (snd larger)

    smallerFront :: Vector (Maybe Unit)
    smallerFront = undefined
