{-# LANGUAGE DeriveDataTypeable #-}
module Riskell.Util where

import Prelude

import Data.Array.IO
import qualified  Data.List as List
import System.Random
import Control.Monad

import Riskell.GameData

-- | if a command was changing the state, we increment the stepCounter
incCounter :: Either String Game -> Either String Game
incCounter = fmap (\g -> g {gameSteps = gameSteps g + 1})


-- | Randomly shuffle a list - from: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- myNewArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    myNewArray :: Int -> [a] -> IO (IOArray Int a)
    myNewArray x = newListArray (1,x)


-- | adds a message to the lastMessages of a game
addMessageToGame :: String -> Game -> Game
addMessageToGame s g = if length (lastMessages g) > 50 then g {lastMessages = (tail $ lastMessages g) ++ [msg] }
                                                       else g {lastMessages = lastMessages g ++ [msg] }
                     where msg = show (gameSteps g) ++ "| " ++ s


-- | this function rotates in an array starting with 0 after the last element
nextIndex :: [a] -> Int -> Int
nextIndex p i | (i+1) == length p = 0
              | otherwise         = i+1

-- | sort a triple based on the Ord
sortTriple :: Ord a => (a,a,a) -> (a,a,a)
sortTriple = listToTriple . List.sort . tripleToList

-- | creates a list out of a triple
tripleToList :: (a,a,a) -> [a]
tripleToList (a,b,c) = [a,b,c]


-- | creates a triple out of a list
listToTriple :: [a] -> (a,a,a)
listToTriple [a,b,c] = (a,b,c)
listToTriple _         = error "this function only works on triples"


-- | roles the dice for the attacker and returns a tuple of three dices
roleAttackerDice :: Int -> Game -> ((Int, Int, Int),Game)
roleAttackerDice 1 g =
    let (a,g1) = diceRoll g
    in  ((a,0,0),g1)
roleAttackerDice 2 g =
    let (a,g1) = diceRoll g
        (b,g2) = diceRoll g1
    in  ((a,b,0),g2)
roleAttackerDice 3 g =
    let (a,g1) = diceRoll g
        (b,g2) = diceRoll g1
        (c,g3) = diceRoll g2
    in  ((a,b,c),g3)
roleAttackerDice _ _ = error "unknown attacker dice counter"


-- | roles the dice for the defender and returns a tuple of two dices
roleDefenderDice :: Int -> Game -> ((Int, Int),Game)
roleDefenderDice 1 g =
    let (a,g1) = diceRoll g
    in  ((a,0),g1)
roleDefenderDice 2 g =
    let (a,g1) = diceRoll g
        (b,g2) = diceRoll g1
    in  ((a,b),g2)
roleDefenderDice _ _ = error "unknown defender dice counter"


-- | risk uses a special logic to decide how many units are lost for two sets of dices
evaluateDices :: (Int,Int,Int) -> (Int,Int) -> (Int,Int)
evaluateDices (a,0,0) (x,y) = if a       >   max x y then (0,1) else (1,0)
evaluateDices (a,b,c) (x,0) = if max a (max b c) > x then (0,1) else (1,0)
evaluateDices (a,b,c) (x,y) =
    let attackerList = reverse $ List.sort $ [a,b,c]
        defenderList = reverse $ List.sort $ [x,y]
        attackerWonFirst  = (attackerList !! 0) > (defenderList !! 0)
        attackerWonSecond = (attackerList !! 1) > (defenderList !! 1)
    in case (attackerWonFirst,attackerWonSecond) of
        (True, True)   -> (0,2)
        (True, False)  -> (1,1)
        (False,True)   -> (1,1)
        (False,False)  -> (2,0)


-- | returns a random card of the set of cards available
getRandomCard :: Game -> (Card,Game)
getRandomCard g = (newCard,newGame)
    where rnd                = mkStdGen (randomSeed g)
          (rndNumber,newGen) = randomR (0::Int,length allCards-1) rnd
          newCard            = allCards !! rndNumber
          (newSeed,_)        = random newGen
          newGame            = g {randomSeed = newSeed}


-- | returns a value between 1 and 6 as a normal dice also would do
diceRoll:: Game -> (Int,Game)
diceRoll g = (rndNumber,newGame)
    where rnd                = mkStdGen (randomSeed g)
          (rndNumber,newGen) = randomR (1::Int,6::Int) rnd
          (newSeed,_)        = random newGen
          newGame            = g {randomSeed = newSeed}