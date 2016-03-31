module Riskell.AI where

import Prelude

import Data.List as List
import Data.Ord
import Data.Either.Unwrap

import Riskell.GameData
import Riskell.World
import Riskell.Game

{-|
  This module contains all AI functions
-}

-- | debug function to solve a given game with the AI till the end
solveGame :: Game -> Game
solveGame g@(Game{currentState=End _})  = g
solveGame g                             = solveGame $ fromRight $ processGameCommand g $ aiGenerateCommand g


-- | this function is called to ask the AI to send a command to manipulate the state of the game
aiGenerateCommand :: Game -> Command
aiGenerateCommand g@(Game{currentState=InitPlacement }) = aiDoInitPlacement  g
aiGenerateCommand g@(Game{currentState=RoundPlacement}) = aiDoRoundPlacement g
aiGenerateCommand g@(Game{currentState=Attacking won }) = aiDoAttacking      g won
aiGenerateCommand g@(Game{currentState=FinalMove     }) = aiDoFinalMove      g
aiGenerateCommand   (Game{currentState=End       _   }) = NullCommand


-- ########################
-- ###  InitPlacement   ###
-- ########################

-- | do the init placement
aiDoInitPlacement :: Game -> Command
aiDoInitPlacement g = Place $ aiFindBestPlacement g (currentPlayer g)


-- | find the best placement out of a list of options
aiFindBestPlacement :: Game -> Player -> Country
aiFindBestPlacement g p = fst $ maximumBy (comparing snd) $ map (\c -> (c, aiUnitPlacementEvaluation c p g) ) (playerOwnedCountries p g)


-- | evaluate a country as a placement option
aiUnitPlacementEvaluation :: Country -> Player -> Game -> Int
aiUnitPlacementEvaluation c p g =
    let neighbourCount   = length $ getConnections c
        attackers        = getAttackerCountAroundCountry c p g
        friendsNear      = getOwnUnitsCountAroundCountry c p g
        ownUnits         = getUnitsForCountry c g
        continentBorder  = countryIsContinentBorder c
        continentNextAtt = playerOwnsContinentWithinOneAttack p g (getConnections c) $ getContinentForCountry c
        onOwnedContinent = playerOwnsContinent p g $ getContinentForCountry c
    in case (attackers,onOwnedContinent, continentBorder,continentNextAtt) of
        -- fortify yourself even without direct danger
        (0, True , True , _    ) -> 20 - ownUnits
        -- the country has no single attacker next ot it so no need to place a unit here
        (0, _    , _    , _    ) -> -30
        -- i own the continent and country is a strategic boarder
        (_, True , True , _    ) -> attackers + 20 - ownUnits
        -- i almost own a country but the
        (_, False , _    ,True ) -> 100
        -- i own the continent but the country is not strategic relevant -> somewhere in the middle
        (_, True , False, _    ) -> -400
        -- prevent going into a big showdown with small forces
        (_, False, False, _    ) | attackers > 3*ownUnits+2 -> -200
        -- i do not own the continent and it is no boarder -> somewhere in the middle - might be a interesting attack start
        (_, False, False, _    ) -> attackers +  8 - ownUnits + neighbourCount - (friendsNear `div` 3)
        -- i do not own the continent and it is a important border
        (_, False, True , _    ) -> attackers +  3 - ownUnits + neighbourCount - (friendsNear `div` 3)


-- ########################
-- ###  RoundPlacement  ###
-- ########################

-- | send the round placement command
aiDoRoundPlacement :: Game -> Command
aiDoRoundPlacement g = case aiFindBestCardOption g of
    Nothing -> Place $ aiFindBestPlacement g (currentPlayer g)
    Just c  -> UseCards c


-- | this function selects the best cards option for the AI or retuns nothing if no combination is possible
aiFindBestCardOption :: Game -> Maybe (Card,Card,Card)
aiFindBestCardOption g = case map cardBonus $ getValidCardOptions $ getCurrentPlayerCards g of
    [] -> Nothing
    cp -> Just $ snd $ List.maximumBy (comparing fst) cp


-- ########################
-- ###    Attacking     ###
-- ########################

-- | send the best attack command
aiDoAttacking :: Game -> Bool -> Command
aiDoAttacking g wonAttackBefore =
    case aiFindBestAttack g wonAttackBefore (currentPlayer g) of
        Nothing  -> EndRound
        Just com -> com


-- | find a best attack
aiFindBestAttack :: Game -> Bool -> Player -> Maybe Command
aiFindBestAttack g wonBefore p =
    let attackOptions = map (\c -> (c,aiAttackEvaluation g p c)) $ aiPossibleAttackMoves p g
        -- if we won an attack before, we select to only attack things that are easy for us
        filtered = if wonBefore then filter (\(_,value) -> value > 0) attackOptions else attackOptions
    in case filtered of
        [] -> Nothing
        xs -> Just $ fst $ maximumBy (comparing snd) $ xs


-- | evaluates a given attack to tell how usefull it is
aiAttackEvaluation :: Game -> Player -> Command -> Int
aiAttackEvaluation g p (Attack (troops,from,to)) =
    let toUnits           = getUnitsForCountry to g
        continentNextAtt  = playerOwnsContinentWithinOneAttack p g [to] $ getContinentForCountry from
        onOwnedContinent  = playerOwnsContinent p g $ getContinentForCountry from
    in case (onOwnedContinent, continentNextAtt) of
        -- with this attack i will get a continent
        (False, True ) -> 100
        -- attack if we might win
        (_    , _    ) -> troops - (1+toUnits*2)
aiAttackEvaluation _ _ _ = error "unknown state in aiAttackEvaluation"


-- | defines the amount of units that makes most sense for a given attack move
aiAttackDefineUnits :: Game -> Player -> (Country,Country) -> Int
aiAttackDefineUnits g p (from,to) =
    let connectionsToOtherContinent= length $ filter id $ map (\o -> getContinentForCountry o /= getContinentForCountry from) $ getConnections from
        fromUnits         = getUnitsForCountry from g - 1
        toUnits           = getUnitsForCountry to g
        continentBorder   = countryIsContinentBorder from
        onOwnedContinent  = playerOwnsContinent p g $ getContinentForCountry from
    in case (onOwnedContinent, continentBorder) of
        -- i own the continent and this is an important country - keep some units on the continent to be safe IF many connections come in
        (True , True )                         -> min fromUnits $ max 3 $ div fromUnits (min 2 connectionsToOtherContinent)
        -- i am attacking a bigger country, no need to be careful
        (_    , _    ) | toUnits > fromUnits   -> fromUnits
        -- will never happen that i own a continent and have to atack from a non border
        (True , False)                         -> 1
        -- i do not own the continent and it is no boarder -> somewhere in the middle - keep some units on the country but go on
        (False, _    )                         -> fromUnits - 3


-- | list all attack options a player has
aiPossibleAttackMoves :: Player -> Game -> [Command]
aiPossibleAttackMoves p g   =
    [Attack (max 1 $ aiAttackDefineUnits g p (a, b),a,b) |
        a <- playerOwnedCountries p g,
        getUnitsForCountry a g > 1,
        b <- enemyOwnedCountries  p g,
        directConnectionAvailable a b]


-- ########################
-- ###    FinalMove     ###
-- ########################

-- | send the final move
aiDoFinalMove :: Game -> Command
aiDoFinalMove g =
    case aiPossibleFinalMoves (currentPlayer g) g of
        [] -> EndRound
        xs -> fst $ maximumBy (comparing snd) $ xs


-- | find options for a final move
aiPossibleFinalMoves :: Player -> Game -> [(Command,Int)]
aiPossibleFinalMoves p g   =
    [ (Move (max 1 $ div (getUnitsForCountry a g) 2,a,b), div (getUnitsForCountry a g) 2) |
        a <- playerOwnedCountries p g,
        b <- playerOwnedCountries p g,
        -- the other country has more attackers then we have
        getAttackerCountAroundCountry b p g > getAttackerCountAroundCountry a p g,
        -- we do not move from borders
        not $ countryIsContinentBorder a,
        getUnitsForCountry a g > 1,
        directConnectionAvailable a b]
