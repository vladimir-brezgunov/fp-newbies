module Main where

import Lib
import System.Random

main :: IO ()
main = do
  putStrLn "Let's explode some kittens!"
  count <- getPlayersCount
  gen <- newStdGen
  let table = makeGame gen count
  --while Length (tableHands table) > 1
  putStrLn $ showCurrentTable table

getPlayersCount :: IO Int
getPlayersCount = do
  putStr "Enter players count: "
  readLn

showCurrentTable :: Table -> String
showCurrentTable Table {
  currentPlayer = curPlayer,
  tableHands = hands,
  cardsInTurn = inTurn
  } = show curPlayer ++ " " ++ show (hands!!curPlayer) ++ " " ++ show inTurn

startGameLoop :: Table -> Table
startGameLoop table @ Table {
  currentPlayer = curPlayer,
  tableHands = hands
  } = undefined
  -- | = table {currentPlayer = curPlayer + 1 `mod` , 
  --         tableHands = killPlayer curPlayer hands}

killPlayer :: Int -> [Hand] -> [Hand]
killPlayer pos hands = before ++ tail where
  (before, (_:tail)) = splitAt pos hands
