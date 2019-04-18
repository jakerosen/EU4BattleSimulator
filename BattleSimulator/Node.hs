module BattleSimulator.Node where

import BattleSimulator.Unit

data Node =
  Node
  { unit :: Maybe Unit -- Nothing if there is no unit in this spot
  -- I don't think the node needs to find a target.  When I was thinking
  -- about the engageBattleDay function, I realized the target finding and
  -- casualty inflicting are going to happen on the same spot, each day.
  -- There is no actual reason why I should have to save that information for
  -- later.  I think it's quite likely that I will not ultimately need a Node
  -- object.
  -- , target :: Maybe Int -- The ID number of the target node, if this unit
                        -- has a target.
  }
