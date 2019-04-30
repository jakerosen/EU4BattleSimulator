module BattleSimulator.Unit where

import GHC.Generics
import Data.Function
import Control.Lens
import Data.Generics.Product.Any
import BattleSimulator.Phase

data Unit
  = Unit
  { unitType :: UnitType -- artillery can attack from back line for 50% dmg
                         -- cavalry need to be checked against infantry for
                         -- penalties
  , strength :: Int -- number of men, [0-1000]
  , maxMorale :: Double -- max morale is used in morale damage calculation
  , currentMorale :: Double
  , fireMod :: Double -- fire tech mod
  , shockMod :: Double -- shock tech mod
  , combatAbility :: Double -- modifier to this unit's performance, called
                            -- combat ability in game
  , discipline :: Double
  , tactics :: Double -- military tactics
  , flanking :: Int -- flanking ability

  , oFirePips :: Int
  , dFirePips :: Int

  , oShockPips :: Int
  , dShockPips :: Int

  , oMoralePips :: Int
  , dMoralePips :: Int
  } deriving stock (Generic)

data UnitType
  = Infantry
  | Cavalry
  | Artillery
  deriving (Eq)

takeCasualties :: (Int, Double) -> Unit -> Unit
takeCasualties (casualties, morale) unit =
  unit
    & the @"strength" %~ (subtract casualties)
    & the @"currentMorale" %~ (subtract morale)

phaseMod :: Phase -> Unit -> Double
phaseMod phase unit =
  case phase of
    Fire -> fireMod unit
    Shock -> shockMod unit
