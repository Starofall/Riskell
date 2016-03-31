{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Riskell.YesodHandler where

import YesodCore.Import

import qualified  Data.Map as Map

import Riskell.AI


{-|
  This is the main controller for the YESOD web interface
  It is used to render the UI on the client and to call the modify on the gameState

  Therefor it stores the game in an IORef (or filesystem)
    1) loaded the game out of the IORef (or file)
    2) changed with the command send from the client
    3) applied to all AI movements and brought to a state that the user is the next actor
    4) save it back to IORef (or file)

  As we only need a key-value storage a IORef in memory (or file) is a suitable minimal solution.
  In a bigger environment a database might be useful.
-}

-- | show the welcome page
getWelcomePage :: Handler Html
getWelcomePage = do
    defaultLayout $ do
        setTitle "Welcome to Riskell"
        $(widgetFile "welcome")


-- | a simple data type used for the YESOD form to object conversion
data CreateGameConfig = CreateGameConfig {
    onlyAI  :: Bool,
    players :: Int
} deriving (Show)

-- | This handler creates a new game that is stored to the games folder
getCreateGame :: Handler Html
getCreateGame = do
    -- load the gameConfig from the get request
    createGameConfig <- runInputGet $ CreateGameConfig
        <$> ireq boolField "onlyAI"
        <*> ireq intField  "players"
    -- simple assurance that gameId is unique (at least for this project)
    newGameId <- liftIO $ randomRIO (1,10000000)
    -- create a new game and store it
    newGame   <- liftIO $ createGame newGameId (onlyAI createGameConfig) (players createGameConfig)
    writeGame newGame
    -- redirect the user to the new game
    redirect $ RenderGame newGameId

-- | renders an empty gameUI if the gameId was valid
getRenderGame :: Int -> Handler Html
getRenderGame submittedGameId = do
    -- check if the gameId is valid
    maybeGame <- readGame submittedGameId
    case maybeGame of
        -- render the gameUI in the browser / game is not needed as client does an NoCommand to get the data
        Just _ -> defaultLayout $ do
            setTitle "Riskell - Playing the Game"
            $(widgetFile "playfield")
        -- render a error to the browser
        Nothing -> defaultLayout $ do
            setTitle "Unknown game"
            [whamlet|<h1>This game is unknown or corrupt - go back and create a new one...|]

-- | applies the parsed command to the game and sends the applied state of the game as a JSON value to the client
postPlayGame :: Int -> String -> Handler Value
postPlayGame submittedGameId rawCommand = do
    maybeGame <- readGame submittedGameId
    case (parseCommand rawCommand,maybeGame) of
        -- invalid command
        (Nothing,      _        ) -> returnJson (pack "Parsing Error" :: Text)
        -- the gameId was wrong so we send an error
        (Just _,       Nothing  ) -> returnJson (pack "Game Id Error" :: Text)
        -- player move
        (Just command, Just game) | isHuman $ currentPlayer game -> do
            case processGameCommand game command of
                Left applyError       -> returnJson (pack applyError :: Text)
                Right correctNextGame -> writeGame correctNextGame >> returnJson correctNextGame
         -- ai move
        (Just command, Just game) | command == CPUStep || command == NullCommand  -> do
            case processGameCommand game $ aiGenerateCommand game of
                Left  applyError      -> returnJson (pack applyError :: Text)
                Right correctNextGame -> writeGame correctNextGame >> returnJson correctNextGame
        -- error
        (_,           _         ) -> returnJson (pack "Invalid Command" :: Text)

-- | as the user sends his command through the url, we need to manually parse the string to be a valid command
-- the commands are send as strings with the structure "P(Country)" -> Place Country and "A(Country,Country)" -> Attack (Country, Country)
-- if the parsing fails we return Nothing
-- #functor #fmap
parseCommand :: String -> Maybe Command
parseCommand ('N' : _) = Just NullCommand
parseCommand ('E' : _) = Just EndRound
parseCommand ('C' : _) = Just CPUStep
parseCommand ('U' : c) = UseCards <$> readMaybe c
parseCommand ('P' : c) = Place    <$> readMaybe c
parseCommand ('M' : c) = Move     <$> readMaybe c
parseCommand ('A' : c) = Attack   <$> readMaybe c
parseCommand   _       = Nothing


{-|
    the following segment is used to save and load the gameState
-}

-- | config value to enable inMemory or fileSystem storage of the games
useMemoryForStoring :: Bool
useMemoryForStoring = True

-- | writes the gameState to file system
-- #IORefs
writeGame :: Game -> Handler ()
writeGame g = case useMemoryForStoring of
    True  -> do
        yesod <- getYesod
        liftIO $ modifyIORef' (games yesod) (\x -> Map.insert (gameId g) g x)
    False -> do
        let fileName = fileNameForId $ gameId g
        writeFile fileName $ show g


-- | reads the gameState from the file system
-- #IORefs #TryCatch #ErrorHandling #LiftIO
readGame :: Int -> Handler (Maybe Game)
readGame submittedGameId =  case useMemoryForStoring of
    True  -> do
        yesod <- getYesod
        games <- liftIO $ readIORef $ games yesod
        return $ lookup submittedGameId games
    False -> liftIO $ do
        -- just catch does not exit errors
        triedReadText <-  tryJust (guard . isDoesNotExistError) $ readFile $ fileNameForId submittedGameId
        case triedReadText of
            -- tried to read the file failed
            Left _ -> return Nothing
            -- reading successful
            Right loadedText -> return $ readMaybe loadedText


-- | the folder the games are stored into (crossPlatform)
gameFolder :: String
gameFolder = extSeparator : pathSeparator : "games" ++ [pathSeparator]

-- | the gameType (crossPlatform)
gameFileTyp :: String
gameFileTyp = extSeparator : "game"

-- | create the fileName for a given id
fileNameForId :: Int -> String
fileNameForId i = gameFolder ++ show i ++ gameFileTyp
