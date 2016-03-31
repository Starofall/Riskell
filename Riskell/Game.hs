module Riskell.Game (module Riskell.Game) where

import Prelude

import Riskell.GameData     as Riskell.Game
import Riskell.World        as Riskell.Game
import Riskell.Util
import Riskell.Placement
import Riskell.Attacking
import Riskell.FinalMove

{-|
    the riskell game is separated into different states.
    in each stable state (that is stored) the currentPlayer has to do something

    1) the game is launched and the user has to set single units as start values
        -> if (stonesLeft != 0) goto 1 else goto 2
    2) a normal round has started and the user has to distribute its units
        -> user can send UseCards command to add units to counter and remove cards
        -> if all units are placed, goto 3
    3) user can attack a country from another player
        -> if (possibleAttacksLeft==true) goto 3
        -> if userAbort goto 4 (if he won once, he gets a card)
    4) now the user can move an specific amount of units from ONE country to ONE other country
        -> if stillRunning goto 2 else goto 5
    5) the game was won or lost - now we do nothing
-}

-- | main game function - brings a game to a next stage
processGameCommand :: Game -> Command -> (Either String Game)
-- 0) do nothing on a nullCommand
processGameCommand g                                     NullCommand      = Right g
-- 1) the game is in the initial placement and we place a unit on the field
processGameCommand g@(Game{currentState=InitPlacement})  (Place country)  = incCounter $ processInitPlacementPlaceCommand     g country
-- 2) a new round started and we have to place some units + we can use our cards
processGameCommand g@(Game{currentState=RoundPlacement}) (Place country)  = incCounter $ processRoundPlacementPlaceCommand    g country
processGameCommand g@(Game{currentState=RoundPlacement}) (UseCards cards) = incCounter $ processRoundPlacementUseCardsCommand g cards
-- 3) attack other countries
processGameCommand g@(Game{currentState=Attacking _})    (Attack x)       = incCounter $ processAttackingAttackCommand        g x
processGameCommand g@(Game{currentState=Attacking _})    EndRound         = incCounter $ processAttackingEndRoundCommand      g
-- 4) now the user could do one single move or end the round
processGameCommand g@(Game{currentState=FinalMove})      (Move   x)       = incCounter $ processFinalMoveMoveCommand          g x
processGameCommand g@(Game{currentState=FinalMove})      EndRound         = incCounter $ processFinalMoveEndRoundCommand      g
-- 5) if the game is done do nothing
processGameCommand g@(Game{currentState=End _})          _                = Right g
-- return error on invalid combination
processGameCommand _                                     _                = Left "invalid command for current state"
