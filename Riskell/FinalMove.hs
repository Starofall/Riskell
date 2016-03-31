module Riskell.FinalMove where

import Prelude

import Riskell.GameData
import Riskell.Util
import Riskell.World

{-|
    the functions in this file handle the final move before a round ends
-}


-- | user requested to have a last move before the round ends
processFinalMoveMoveCommand :: Game -> (Int,Country,Country) -> Either String Game
processFinalMoveMoveCommand g c@(count,from,to) =
    let p = currentPlayer g in
    case movePossible c p g of
        True  -> Right $ startRoundPlacement $ moveUnitsOnGame from to count $ addMessageToGame (show p ++ " moved " ++ show count ++ " units from " ++ show from ++ " to " ++ show to) g
        False -> Left "move not possible"

-- | user ignored his last move
processFinalMoveEndRoundCommand :: Game -> Either String Game
processFinalMoveEndRoundCommand g = Right $ startRoundPlacement  g

-- | checks if a given attack command is valid for the player and the current game state
movePossible :: (Int,Country,Country) -> Player -> Game -> Bool
movePossible c@(_,from,to) p g = connectedAndUnitsAvailable c g && ownerOK
    where ownerOK = isPlayerCountry from p g && isPlayerCountry to p g

