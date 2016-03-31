module Riskell.Placement where

import Prelude

import qualified  Data.List as List
import qualified  Data.Map as Map

import Riskell.GameData
import Riskell.Util
import Riskell.World


{-|
    the functions in this file handle both the initial placement and the placement before a new round starts
-}


-- | processes the command that the user has given to set a unit in the initialPlacement
processInitPlacementPlaceCommand :: Game -> Country -> Either String Game
processInitPlacementPlaceCommand g c = let p = currentPlayer g in
    case (isPlayerCountry c p g, placeOneUnit p c g) of
        (True,  playerPlaced) | openPlacements playerPlaced <= 0 -> Right $ startRoundPlacement playerPlaced
        (True,  playerPlaced) -> Right $ playerPlaced {currentPlayer = (getNextPlayer playerPlaced)}
        (_   ,  _           ) -> Left "not owned by player -> cheating"

-- ##########################################################

-- | when a new round starts the player has to place all units until openPlacement is 0
processRoundPlacementPlaceCommand :: Game -> Country -> Either String Game
processRoundPlacementPlaceCommand g c = let p = currentPlayer g in
    case (isPlayerCountry c p g, placeOneUnit p c g) of
        (True, playerPlacedGame) | openPlacements playerPlacedGame <= 0 -> Right $ startAttacking playerPlacedGame
        (True, playerPlacedGame) -> Right playerPlacedGame
        (_   , _               ) -> Left "not owned by player -> cheating"

-- | in the placement phase the user decided to use its cards to increase its unit counter
processRoundPlacementUseCardsCommand :: Game -> (Card,Card,Card) -> Either String Game
processRoundPlacementUseCardsCommand g cardCombination =
    let -- as the human player can select any order for the cards, we need to sort them first
        sortedCombination = sortTriple cardCombination
        -- checks if the combination is element of all valid card options for a player
        combinationValid  = List.elem sortedCombination $ getValidCardOptions (getCurrentPlayerCards g)
    in case combinationValid of
        True  ->
            let -- find out how many points it creates
                (bonusPoints,_)  = cardBonus sortedCombination
                -- remove the cards from the playerList and update the game
                newCardsOfPlayer = (getCurrentPlayerCards g) List.\\ (tripleToList sortedCombination)
                newPlayerCards   = Map.insert (currentPlayer g) newCardsOfPlayer (playerCards g)
                nextGame         = g {playerCards=newPlayerCards, openPlacements=openPlacements g + bonusPoints}
                -- update the game and add the bonus points
             in Right $ addMessageToGame (show (currentPlayer nextGame) ++ " used cards and gained " ++ show bonusPoints ++ " additional units ") $ nextGame
        -- | the user send an invalid
        False -> Right g
