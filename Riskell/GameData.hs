{-# LANGUAGE DeriveGeneric #-}
-- this increases the compilation time a lot - but is needed for JSON
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- fno-warn-orphans is needed for JSON instances

module Riskell.GameData where

import Prelude

import Data.Map as Map

-- used for json
import Data.Aeson
import GHC.Generics
-- /end used for json

-- | possible states a game can be in when it gets stored to file system
data GameState = InitPlacement  | -- game started and placement is needed
                 RoundPlacement | -- start of a round where all units need to be placed
                 Attacking Bool | -- user is about to attack other countries (bool tells if the user has won a battle in a attack this round)
                 FinalMove      | -- user finished attacking and has the option to move one army
                 End Player       -- game has ended - player has won
 deriving(Read,Show,Eq,Generic)

instance FromJSON GameState
instance ToJSON GameState where
  toJSON = genericToJSON defaultOptions

-- | a command is something that is applied to the gameState
data Command = NullCommand                   | -- user has not send a command but gets an update on the game
               Place Country                 | -- place a unit on the specific country
               UseCards (Card,Card,Card)     | -- use the cards and increase the placementCounter
               EndRound                      | -- finishes a move or attack round
               Attack (Int,Country,Country)  | -- attacks an enemy country using x amount of units (they also move there after win)
               Move   (Int,Country,Country)  | -- moves x units from a to b
               CPUStep                         -- if the game is in an AI state this triggers a processing (allowing slow progress in AI games)
 deriving(Eq,Read,Ord,Show,Generic)

instance FromJSON Command
instance ToJSON Command where
  toJSON = genericToJSON defaultOptions

-- | the game contains all state information needed to play riskell
data Game = Game {
    gameSteps      :: Int,                     -- steps the game has taken
    gameId         :: Int,                     -- unique id for each game
    openPlacements :: Int,                     -- the units that are still to place until the next stage
    currentState   :: GameState,               -- state of the game
    gameMap        :: Map Country CountryState,-- state for each country
    playerList     :: [Player],                -- list of players
    playerCards    :: Map Player [Card],       -- cards the players currently own (we use int for better serialization)
    lastMessages   :: [String],                -- used to display what happened in the game to the player
    randomSeed     :: Int,                     -- a seed used to generate future random numbers
    currentPlayer  :: Player                   -- the user that has to send the next command
} deriving (Eq,Read,Show,Generic)

instance FromJSON Game
instance ToJSON Game where
  toJSON = genericToJSON defaultOptions

-- | a countryState describes the state of a country in the world of the game
data CountryState = CountryState {
    associatedCountry  :: Country,      -- the name of the country this state is associated to
    occupyingPlayer    :: Maybe Player, -- the player currently owning this country
    unitCount          :: Int           -- the amount of units on this country
} deriving (Show,Eq,Read,Generic)

instance FromJSON CountryState
instance ToJSON CountryState where
    toJSON = genericToJSON defaultOptions

-- | a player is a participant of the riskell game
data Player = Human | CPU1 | CPU2 | CPU3 | CPU4 | CPU5
    deriving(Ord, Read, Show,Generic,Eq)

instance FromJSON Player
instance ToJSON Player where
    toJSON = genericToJSON defaultOptions

-- | player list used for a game with a human player
humanPlayerList :: [Player]
humanPlayerList = [Human, CPU1, CPU2, CPU3, CPU4]

-- | player list used for a game without a human
aiPlayerList :: [Player]
aiPlayerList = [CPU1, CPU2, CPU3, CPU4, CPU5]

-- | used to test if a currentPlayer is the human player
isHuman :: Player -> Bool
isHuman Human = True
isHuman _     = False

-- | config for the startupUnits
calculateStartUnits :: Int -> Int
calculateStartUnits 3 = 3*35
calculateStartUnits 4 = 4*30
calculateStartUnits 5 = 5*25
calculateStartUnits _ = 30

-- to implement the JSON transformation of maps we need this
-- #fmap
instance FromJSON v => FromJSON (Map Country v) where
 parseJSON = fmap (Map.mapKeys read) . parseJSON
instance FromJSON v => FromJSON (Map Int v) where
 parseJSON = fmap (Map.mapKeys read) . parseJSON
instance FromJSON v => FromJSON (Map Player v) where
 parseJSON = fmap (Map.mapKeys read) . parseJSON
instance ToJSON v => ToJSON (Map Int v) where
 toJSON = toJSON . Map.mapKeys show
instance ToJSON v => ToJSON (Map Country v) where
 toJSON = toJSON . Map.mapKeys show
instance ToJSON v => ToJSON (Map Player v) where
 toJSON = toJSON . Map.mapKeys show

-- | The cards a player can own
data Card = Soldier | Cavalier | Cannon | Joker
 deriving (Show,Eq,Ord,Enum,Read,Generic)

instance FromJSON Card
instance ToJSON Card where
    toJSON = genericToJSON defaultOptions

-- | All cards in a list
allCards :: [Card]
allCards = [Soldier ..]

-- | calculates the game bonus for a given combination of cards
cardBonus :: (Card,Card,Card) -> (Int,(Card,Card,Card))
-- soldier = 4
cardBonus c@(Soldier,  Soldier,  Soldier ) = (4,c)
cardBonus c@(Soldier,  Soldier,  Joker   ) = (4,c)
cardBonus c@(Soldier,  Joker,    Joker   ) = (4,c)
-- cavalier = 6
cardBonus c@(Cavalier, Cavalier, Cavalier) = (6,c)
cardBonus c@(Cavalier, Cavalier, Joker   ) = (6,c)
cardBonus c@(Cavalier, Joker,    Joker   ) = (6,c)
-- Cannon only = 8
cardBonus c@(Cannon,   Cannon,   Cannon  ) = (8,c)
cardBonus c@(Cannon,   Cannon,   Joker   ) = (8,c)
cardBonus c@(Cannon,   Joker,    Joker   ) = (8,c)
-- mixed series = 10
cardBonus c@(Soldier,  Cavalier, Cannon  ) = (10,c)
cardBonus c@(Joker,    Joker,    Joker   ) = (10,c)
-- all other = 0
cardBonus c@(_,        _,        _       ) = (0,c)

-- | a list of all possible combinations for a valid cardBonus - sorted by natural order
allValidCardCombinations :: [(Card,Card,Card)]
allValidCardCombinations = [
            (Soldier, Soldier, Soldier),   (Soldier, Soldier, Joker),   (Soldier, Joker, Joker),
            (Cavalier, Cavalier, Cavalier),(Cavalier, Cavalier, Joker), (Cavalier, Joker, Joker),
            (Cannon, Cannon, Cannon),      (Cannon, Cannon, Joker),     (Cannon, Joker, Joker),
            (Soldier, Cavalier, Cannon),   (Joker, Joker, Joker)
           ]

-- | a continent is a key and a defined group of countries
data Continent = Australia | Europe | NorthAmerica | SouthAmerica | Asia | Africa
 deriving (Show,Enum,Eq,Ord,Read)

-- | a list of all continents
allContinents :: [Continent]
allContinents = [Australia ..]

-- | a country is a defined key for a countryState value. it is used to create the worldGraph at compile time.
data Country   = Alaska | NorthWestTerritory | Alberta | Ontario | Quebec | Greenland | WestStates | EastStates | MiddleAmerica |
                 Venezuela | Peru | Brazil | Argentina |
                 Iceland | GreatBritain | Scandinavia | MiddleEurope | WestEurope | SouthEurope | Ukraine |
                 NorthWestAfrica | Agypt | EastAfrica | Kongo | SouthAfrica | Madagascar |
                 MiddleEast | Afghanistan | Ural | Siberia | Jakutien | Kamchatka | Irkutsk | Mongol | China | India | Siam | Japan |
                 Indonesia | NewGuinea | WestAustralia | EastAustralia
 deriving (Show,Enum,Eq,Ord,Read,Generic)

instance FromJSON Country
instance ToJSON Country where
    toJSON = genericToJSON defaultOptions

-- | a list of all countries
allCountries  :: [Country]
allCountries  = [Alaska ..]

-- | a countryConfig is used for the configFile to describe the name of a country and its connections
data CountryConfig = CountryConfig {
    continent   :: Continent,  -- the continent of the country
    shortString :: String,   -- the short name of the country
    connections :: [Country] -- the connections to other countries
}

-- | country definitions
countryConfig :: Map.Map Country CountryConfig
countryConfig =  Map.fromList [
    --North America
    (Alaska,            (CountryConfig NorthAmerica "AL" [Kamchatka, NorthWestTerritory, Alberta])),
    (NorthWestTerritory,(CountryConfig NorthAmerica "NW" [Alaska, Alberta, Ontario, Greenland])),
    (Alberta,           (CountryConfig NorthAmerica "AB" [Alaska, NorthWestTerritory, Ontario, WestStates])),
    (Ontario,           (CountryConfig NorthAmerica "OT" [NorthWestTerritory, Alberta, WestStates, EastStates, Quebec, Greenland])),
    (Quebec,            (CountryConfig NorthAmerica "QB" [Ontario, EastStates, Greenland])),
    (Greenland,         (CountryConfig NorthAmerica "GR" [NorthWestTerritory, Ontario, Quebec, Iceland])),
    (WestStates,        (CountryConfig NorthAmerica "WS" [Alberta, Ontario, EastStates, MiddleAmerica])),
    (EastStates,        (CountryConfig NorthAmerica "ES" [Ontario, WestStates, MiddleAmerica, Quebec])),
    (MiddleAmerica,     (CountryConfig NorthAmerica "MA" [WestStates, Venezuela, EastStates])),
    --South America
    (Venezuela,         (CountryConfig SouthAmerica "VA" [MiddleAmerica, Peru, Brazil])),
    (Peru,              (CountryConfig SouthAmerica "PU" [Venezuela, Brazil, Argentina])),
    (Brazil,            (CountryConfig SouthAmerica "BR" [Venezuela, Peru, Argentina, NorthWestAfrica])),
    (Argentina,         (CountryConfig SouthAmerica "AG" [Peru, Brazil])),
    --Europe
    (Iceland,           (CountryConfig Europe "IC" [Greenland, GreatBritain, Scandinavia])),
    (GreatBritain,      (CountryConfig Europe "GB" [Iceland, WestEurope, MiddleEurope, Scandinavia])),
    (Scandinavia,       (CountryConfig Europe "SC" [Iceland, GreatBritain, MiddleEurope, Ukraine])),
    (MiddleEurope,      (CountryConfig Europe "ME" [Scandinavia, GreatBritain, WestEurope, SouthEurope, Ukraine])),
    (WestEurope,        (CountryConfig Europe "WE" [GreatBritain, NorthWestAfrica, SouthEurope, MiddleEurope])),
    (SouthEurope,       (CountryConfig Europe "SE" [MiddleEurope, WestEurope, NorthWestAfrica, Agypt, MiddleEast, Ukraine])),
    (Ukraine,           (CountryConfig Europe "UK" [Scandinavia, MiddleEurope, SouthEurope, MiddleEast, Afghanistan, Ural])),
    --Asia
    (MiddleEast,        (CountryConfig Asia "EE" [Ukraine, SouthEurope, Agypt, India, Afghanistan])),
    (Afghanistan,       (CountryConfig Asia "AF" [Ukraine, MiddleEast, India, China, Ural])),
    (Ural,              (CountryConfig Asia "UR" [Ukraine, Afghanistan, China, Siberia])),
    (Siberia,           (CountryConfig Asia "SI" [Ural, China, Mongol, Irkutsk, Jakutien])),
    (Jakutien,          (CountryConfig Asia "JA" [Siberia, Irkutsk, Kamchatka])),
    (Kamchatka,         (CountryConfig Asia "KM" [Jakutien, Irkutsk, Mongol, Japan, Alaska])),
    (Irkutsk,           (CountryConfig Asia "IR" [Siberia, Mongol, Kamchatka, Jakutien])),
    (Mongol,            (CountryConfig Asia "MO" [Irkutsk, Siberia, China, Japan, Kamchatka])),
    (China,             (CountryConfig Asia "CH" [Siberia, Ural, Afghanistan, India, Siam, Mongol])),
    (India,             (CountryConfig Asia "IN" [Afghanistan, MiddleEast, Siam, China])),
    (Siam,              (CountryConfig Asia "SM" [China, India, Indonesia])),
    (Japan,             (CountryConfig Asia "JP" [Mongol, Kamchatka])),
    --Australia
    (Indonesia,         (CountryConfig Australia "ID" [Siam, NewGuinea, WestAustralia])),
    (NewGuinea,         (CountryConfig Australia "NG" [Indonesia, WestAustralia, EastAustralia])),
    (WestAustralia,     (CountryConfig Australia "WA" [Indonesia, NewGuinea, EastAustralia])),
    (EastAustralia,     (CountryConfig Australia "EA" [WestAustralia, NewGuinea])),
    --Africa
    (NorthWestAfrica,   (CountryConfig Africa "NF" [WestEurope, Brazil, Kongo, EastAfrica, Agypt, SouthEurope])),
    (Agypt,             (CountryConfig Africa "AY" [SouthEurope, NorthWestAfrica, EastAfrica, MiddleEast])),
    (EastAfrica,        (CountryConfig Africa "EF" [Agypt, Kongo, SouthAfrica, Madagascar, NorthWestAfrica])),
    (SouthAfrica,       (CountryConfig Africa "SF" [Kongo, Madagascar, EastAfrica])),
    (Madagascar,        (CountryConfig Africa "MG" [EastAfrica, SouthAfrica])),
    (Kongo,             (CountryConfig Africa "KG" [NorthWestAfrica, SouthAfrica, EastAfrica]))
    ]

-- | a continentConfig is used to define the attributes of a continent
data ContinentConfig = ContinentConfig {
    continentBonusPoints :: Int  -- the points a user gets extra if he owns a complete continent
}

-- | continent definitions
continentConfig :: Map.Map Continent ContinentConfig
continentConfig = Map.fromList [
    (NorthAmerica, (ContinentConfig 5)),
    (SouthAmerica, (ContinentConfig 2)),
    (Europe,       (ContinentConfig 5)),
    (Asia,         (ContinentConfig 7)),
    (Australia,    (ContinentConfig 2)),
    (Africa,       (ContinentConfig 3))
    ]



