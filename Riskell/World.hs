module Riskell.World where

import Prelude

import qualified Data.Map as Map
import Data.List as List
import Data.Function.Flippers
import Data.Maybe
import System.Random

import Riskell.Util
import Riskell.GameData

-- #######
-- Getters
-- #######

-- | returns true if the country is owned not by the player OR unassigned
isEnemyCountry :: Country -> Player -> Game -> Bool
isEnemyCountry c p = isEnemy . occupyingPlayer . getCountryStateForCountry c
    where isEnemy (Just otherPlayer) = p /= otherPlayer
          isEnemy Nothing            = False

-- | returns true if the player owns the country
isPlayerCountry :: Country -> Player -> Game -> Bool
isPlayerCountry c p = isPlayer . occupyingPlayer . getCountryStateForCountry c
    where isPlayer (Just otherPlayer) = p == otherPlayer
          isPlayer Nothing            = False

-- | returns all countries owned by an enemy OR unassigned
enemyOwnedCountries :: Player -> Game -> [Country]
enemyOwnedCountries p g = filter (flip3 isEnemyCountry g p) allCountries

-- | returns the list of countries owned by the player
playerOwnedCountries :: Player -> Game -> [Country]
playerOwnedCountries p g  = filter (flip3 isPlayerCountry g p) allCountries

-- | returns all countryStages (owned countries) for a given player
getCountryStatesForPlayer :: Player -> Game -> [CountryState]
getCountryStatesForPlayer p = filter ((Just p ==).occupyingPlayer) . Map.elems . gameMap

-- | returns true if there is a direct connection between the countries
directConnectionAvailable :: Country -> Country -> Bool
directConnectionAvailable a b = a `elem` getConnections b

-- | returns the country state for a requested country
getCountryStateForCountry :: Country -> Game -> CountryState
getCountryStateForCountry country = fromJust . Map.lookup country . gameMap

-- | returns the units for a given country
getUnitsForCountry :: Country -> Game -> Int
getUnitsForCountry c = unitCount . getCountryStateForCountry c

-- | returns the total number of units a user has on all countries
getTotalUnitsForPlayer :: Player -> Game -> Int
getTotalUnitsForPlayer p g = sum $ map (($ g) . getUnitsForCountry) (playerOwnedCountries p g)

-- | returns the total number of units of all enemy countries around a given country
getAttackerCountAroundCountry :: Country -> Player -> Game -> Int
getAttackerCountAroundCountry c p g = sum $ map (`getUnitsForCountry` g) $ filter (flip3 isEnemyCountry g p) (getConnections c)

-- | returns the total number of units of all player countries around a given country
getOwnUnitsCountAroundCountry :: Country -> Player -> Game -> Int
getOwnUnitsCountAroundCountry c p g = sum $ map (`getUnitsForCountry` g) $ filter (flip3 isPlayerCountry g p) (getConnections c)

-- | iterates over the playerList and returns the next player (we can guarantee that the player is in the list, so we use fromJust
getNextPlayer :: Game -> Player
getNextPlayer g =  list !! nextIndex list (fromJust $ elemIndex (currentPlayer g) list)
    where list = playerList g

-- | returns the amount of bonusPoints the user gets for owning the continent
getBonusPointsForContinent :: Continent -> Int
getBonusPointsForContinent c = continentBonusPoints $ continentConfig Map.! c

-- | returns the continent the country is part of
getContinentForCountry :: Country -> Continent
getContinentForCountry = continent . fromJust . flip Map.lookup countryConfig

-- | returns all countries that are part of a continent
getCountriesForContinent :: Continent -> [Country]
getCountriesForContinent c = selectFirst $ filterRequestedContinent countryTupel
    where selectFirst = map fst
          filterRequestedContinent = filter ((c==).snd)
          countryTupel = map (\x -> (x,getContinentForCountry x)) allCountries

-- | returns all countries that are connected to the given continent
getConnections :: Country -> [Country]
getConnections = connections . fromJust . flip Map.lookup countryConfig

-- | returns the cards for the current player
getCurrentPlayerCards :: Game -> [Card]
getCurrentPlayerCards g = (playerCards g) Map.! (currentPlayer g)

-- | true if a given player owns a complete continent
playerOwnsContinent :: Player-> Game -> Continent -> Bool
playerOwnsContinent p g c = ownedCountryLength == neededCountryLength
    where playerCountryList        = playerOwnedCountries p g
          allCountriesForContinent = getCountriesForContinent c
          ownedCountryLength       = length (playerCountryList `intersect` allCountriesForContinent)
          neededCountryLength      = length allCountriesForContinent

-- | true if a given player owns a complete continent BUT ONE country is missing
playerOwnsContinentWithinOneAttack :: Player-> Game -> [Country] -> Continent -> Bool
playerOwnsContinentWithinOneAttack p g list c = length ( (list `union` playerCountryList) `intersect` allCountriesForContinent) == length allCountriesForContinent
    where playerCountryList        = playerOwnedCountries p g
          allCountriesForContinent = getCountriesForContinent c

-- | true if the country is a continent boarder
countryIsContinentBorder :: Country -> Bool
countryIsContinentBorder c = any id $ map (\o -> getContinentForCountry o /= getContinentForCountry c) $ getConnections c

-- | true if from and to are connected and from has more than amount units
connectedAndUnitsAvailable :: (Int,Country,Country) -> Game -> Bool
connectedAndUnitsAvailable (amount,from,to) g = connected && unitsAvailable
    where connected      = directConnectionAvailable from to
          unitsAvailable = amount>0 && amount<getUnitsForCountry from g

-- | get all valid card options for a given card list
getValidCardOptions :: [Card] -> [(Card,Card,Card)]
getValidCardOptions cl = List.filter (isPossibleCombination cl) allValidCardCombinations
    where -- checks if a given card combination can be created out of the userCards
          isPossibleCombination :: [Card] -> (Card,Card,Card) -> Bool
          isPossibleCombination userCards combination = requiredSize == realSize
            where realSize     = length $ userCards List.\\ (tripleToList combination)
                  requiredSize = length userCards - 3 -- three less then before means all were included


-- #########
-- Modifiers
-- #########

-- | moves int amount of units from country a to country b
moveUnitsOnGame :: Country -> Country -> Int -> Game -> Game
moveUnitsOnGame from to amount = modifyCountryState from (modifyUnits (subtract amount)) . modifyCountryState to (modifyUnits (+ amount))

-- | modifies a countryState by applying a function to the unitCount
modifyUnits :: (Int->Int) -> CountryState -> CountryState
modifyUnits f c = c {unitCount = f $ unitCount c}

-- | modifies the countryState by applying a function to the player
modifyCountryStatePlayer :: (Maybe Player->Maybe Player) -> CountryState -> CountryState
modifyCountryStatePlayer f c = c {occupyingPlayer = f $ occupyingPlayer c}

-- | this method applies the change function to the selected country and returns a new world
modifyCountryState :: Country -> (CountryState->CountryState) -> Game -> Game
modifyCountryState c f g = g {gameMap = Map.adjust f c $ gameMap g}

-- #########
-- Game
-- #########

-- | called from YESOD to create a new game
createGame :: Int -> Bool -> Int -> IO Game
createGame gameId aiOnly playerNr = do
    -- first we shuffle the countryList to simple distribute the start-countries
    shuffledCountries <- shuffle allCountries
    -- generate a new random seed
    seed <- randomIO
    let playerList = case aiOnly of
            True  -> take playerNr aiPlayerList
            False -> take playerNr humanPlayerList
    -- assign the players to the world and return the created gameWorld
    return $ addMessageToGame ("Game started!") $ initAssignCountries shuffledCountries gameId seed playerList


-- | initial sets who owns which country and adds one unit
initAssignCountries :: [Country] -> Int -> Int -> [Player] -> Game
initAssignCountries shuffledCountries gameId seed playerList = Game 0 gameId startUnits InitPlacement startCountriesStates playerList startCardList [] seed (head playerList)
    where startCountriesStates = applyPlayers shuffledCountries 0 Map.empty
          startUnits           = calculateStartUnits (length playerList)
          startCardList        = Map.fromList $ map (\p -> (p,[])) playerList
          -- apply the players to the world
          applyPlayers :: [Country]->Int->Map.Map Country CountryState->Map.Map Country CountryState
          applyPlayers []     _       = id
          applyPlayers (x:xs) counter = applyPlayers xs (nextIndex playerList counter) . Map.insert x (CountryState x (Just (playerList !! counter)) 1)


-- | places a unit on the country and adds a message to the game
placeOneUnit :: Player -> Country -> Game -> Game
placeOneUnit p c g = addMessageToGame messageString $ decreaseOpenPlacementsCounter $ addOneUnit g
     where decreaseOpenPlacementsCounter x = x {openPlacements = openPlacements x - 1}
           addOneUnit = modifyCountryState c $ modifyUnits  (+1)
           messageString = show p ++ " placed one unit on " ++ show c

-- | after initPlacement or the beginning of a new round this method sets up the state for a new userRound
startRoundPlacement :: Game -> Game
startRoundPlacement g = addMessageToGame msg $ setOpenUnitPlacement $ setStageRoundPlacement g
    where nextPlayer = getNextPlayer g
          msg = show nextPlayer ++ " started round with " ++ show unitCounter ++ " extra units"
          -- based on the rules the minimum amount of units is three
          unitCounter = max 3 $ calculateTotalBonus nextPlayer g
          -- sets current player and openPlacements
          setOpenUnitPlacement :: Game -> Game
          setOpenUnitPlacement x = x {openPlacements = unitCounter, currentPlayer = nextPlayer}
          -- set the game to RoundPlacement state
          setStageRoundPlacement :: Game -> Game
          setStageRoundPlacement x = x {currentState = RoundPlacement}

-- | brings the game into the Attacking state
startAttacking :: Game -> Game
startAttacking g = g {currentState = Attacking False}

-- | brings the game into the FinalMove state
startFinalMove :: Game -> Game
startFinalMove g = g {currentState = FinalMove}

-- #########
-- Calculations
-- #########

-- | calculates the total bonusPoints a user gets
calculateTotalBonus :: Player -> Game -> Int
calculateTotalBonus p g = calculateCountryBonus p g + calculateContinentBonus p g

-- | calculates the bonus for the countries
calculateCountryBonus :: Player -> Game -> Int
calculateCountryBonus p g = length (getCountryStatesForPlayer p g) `div` 3

-- | calculates the continent bonus
calculateContinentBonus :: Player -> Game -> Int
calculateContinentBonus p g = sum $ map checkContinent allContinents
    where -- returns the points for a owned continent
          checkContinent :: Continent -> Int
          checkContinent c | playerOwnsContinent p g c = getBonusPointsForContinent c
          checkContinent _                             = 0