module Riskell.Attacking where

import Prelude

import qualified Data.Map as Map
import qualified Data.List as List

import Riskell.GameData
import Riskell.Util
import Riskell.World

{-|
    the functions in this file handle the attacking state, in which the player attacks other countries
-}


-- | the user decided to attack a country
processAttackingAttackCommand :: Game -> (Int,Country,Country) -> (Either String Game)
processAttackingAttackCommand g c@(attackers,from,to) = let p = currentPlayer g in
    case attackPossible c p g of
        True ->
            let addedGame = addMessageToGame (show p ++ " attacked " ++ show to ++ " from " ++ show from) g
                -- do the attackPhase
                (game,wonAttack)     = attackPhase p from to addedGame attackers
                -- look for players with no country associated to them
                playerAndCountryList = map (\px -> (px,playerOwnedCountries px game) ) (playerList game)
                -- list of players with no country
                playersWithNoCountry = map fst $ filter (\(_,list) -> null list) $ playerAndCountryList
                -- the map of cards
                cardsMap             = playerCards game
                -- cards the player has
                cardsOfPlayer        = cardsMap Map.! p
                -- cards the killed player has
                cardsOfKilledPlayer  = concatMap ((playerCards g) Map.!) playersWithNoCountry
                -- new new list of players reduced by them with no country
                newPlayerList        = (playerList game) List.\\ playersWithNoCountry
                -- the new map of cards
                newCardsMap          = Map.filterWithKey (\x _ -> List.elem x newPlayerList) $ Map.insert p (cardsOfKilledPlayer ++ cardsOfPlayer) cardsMap
                -- the new game with new playerList and new playerCards
                correctedGame        = game {playerList = newPlayerList, playerCards = newCardsMap}
                -- if the playerList now only contains one element, set the game to Won if we are the last one else to Lost
            in case (wonAttack,length newPlayerList == 1) of
                -- only one player left
                (_   , True) -> Right $ addMessageToGame ("Game Ended - Winner: " ++ show (head newPlayerList)) $ correctedGame {currentState = End $ head newPlayerList}
                -- we won one fight so set the state to true
                (True, _   ) -> Right $ correctedGame {currentState = Attacking True}
                -- we did not won this round, but we did maybe before
                (False,_   ) -> Right correctedGame
        False -> Left $ "attack move not possible: " ++ show p ++ " wanted to :" ++ show c

-- | user finished his attack on other countries
processAttackingEndRoundCommand :: Game -> (Either String Game)
processAttackingEndRoundCommand g = let p = currentPlayer g in
    case currentState g of
        -- check if the user has won a battle in this round
        -- if yes give him a random card
        Attacking True  -> do
            let (card,newGame) = getRandomCard g
                newMap = Map.adjust (\list -> card:list) p (playerCards g)
                newCardGame = newGame {playerCards = newMap}
             in Right $ startFinalMove newCardGame
        -- if not do not
        Attacking False -> Right $ startFinalMove g
        _               -> error "unknown state in processAttackingEndRoundCommand"


-- | checks if a given attack command is valid for the player and the current game state
attackPossible :: (Int,Country,Country) -> Player -> Game -> Bool
attackPossible c@(_,from,to) p g = connectedAndUnitsAvailable c g && ownerOK
    where ownerOK = isPlayerCountry from p g && isEnemyCountry to p g

-- | this models the attack of two countries with a given number of attackers
attackPhase :: Player -> Country -> Country -> Game -> Int -> (Game,Bool)
attackPhase p from to g totalAttackingUnits =
    let -- limit attackers per run to 3 + keep one unit at the homeBase
        attackers = min totalAttackingUnits (min 3 $ getUnitsForCountry from g - 1)
        -- limit defenders per run to 2
        defenders = min 2 $ getUnitsForCountry to g
        -- roll the dices
        ((attackerLosing, defenderLosing), g2) = doDiceRolling attackers defenders g
        messagedGame = addMessageToGame ("Fight - Attacker Lost: " ++ show attackerLosing ++ " - Defender Lost: " ++ show defenderLosing ) g2
        -- reduce the number of units on both sides
        newGame = modifyCountryState to (modifyUnits (subtract defenderLosing)) $ modifyCountryState from (modifyUnits (subtract attackerLosing)) $ messagedGame
    in case (getUnitsForCountry to newGame == 0,(getUnitsForCountry from newGame) == 1 || (totalAttackingUnits - attackerLosing <= 0)) of
        (True, _    )-> do
            let -- set ownership of won country
                changedOwnerShipGame = modifyCountryState to (modifyCountryStatePlayer (\_->Just p)) newGame
                -- check how many units the player still can move to the target
                stillMoveableUnits = min totalAttackingUnits $ getUnitsForCountry from changedOwnerShipGame - 1
                -- move the requested units
                movedWorld = moveUnitsOnGame from to stillMoveableUnits changedOwnerShipGame
             in (movedWorld,True)
        -- check if the attacking country is only filled with one unit OR the attackers are zero or less
        (False, True ) -> (newGame,False)
        (False, False) -> attackPhase p from to newGame (totalAttackingUnits - attackerLosing)

-- | roles the dice and returns a number of dead attackers and defenders
doDiceRolling :: Int -> Int -> Game ->((Int,Int),Game)
doDiceRolling attackers defenders g =
    let -- role the dices
        (attDice,g1) = roleAttackerDice attackers g
        (defDice,g2) = roleDefenderDice defenders g1
        -- evaluate the results
    in  (evaluateDices attDice defDice, g2)