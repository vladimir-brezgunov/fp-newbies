module Lib where

import System.Random

data Card = Explosion | Defuse | Favor | Skip | Shuffle | Attack |
            Nope | SeeTheFuture | NoInstruction Kind
                                  deriving Show
type Kind = String

type Hand = (Int, [Card])

type Deck = [Card]

data Table = Table { tableDeck :: Deck,
                     tableDiscard :: [Card],
                     tableHands :: [Hand],
                     cardsInTurn :: [Card],
                     currentPlayer :: Int }
  deriving Show

defaultDeck = unpack [(Explosion, 4), (Defuse, 6), (Favor, 4), (Skip, 4),
               (Shuffle, 4), (Attack, 4), (Nope, 4), (SeeTheFuture, 4),
               (NoInstruction "1", 4), (NoInstruction "2", 4), (NoInstruction "3", 4),
               (NoInstruction "4", 4), (NoInstruction "5", 4)]

unpack :: [(Card, Int)] -> Deck
--unpack = concatMap (\ (card, count) -> replicate count card)
unpack = concatMap (uncurry $ flip replicate)
  
shuffle :: StdGen -> Deck -> Deck
shuffle _ [] = []
shuffle stdGen cards = randomCard:shuffle newGen rest where
  (left, randomCard:right) = splitAt index cards
  rest = left ++ right
  (index, newGen) = randomR (0, length cards - 1) stdGen

distributeDeck :: Int -> [Card] -> (Deck, [[Card]])
distributeDeck 0 cards = (cards, [])
distributeDeck numPlayers cards = (deck, firstHand:hands) where
  numCards = 5
  (firstHand, restCards) = splitAt numCards cards
  (deck, hands) = distributeDeck (numPlayers - 1) restCards

makeGame :: StdGen -> Int -> Table
makeGame _ playersNum | playersNum < 2 = error "Get me players, please"
                      | playersNum > 5 = error "Too many players!"
makeGame gen playersNum = Table deck [] (zip [0..] hands) [] startPlayer where
  (deck, hands) = distributeDeck playersNum $ shuffle gen defaultDeck
  startPlayer = fst $ randomR (0, playersNum - 1) gen 

resolveTurn :: Table -> Table
resolveTurn (Table (topCard:deck) discard hands playedCards currentPlayer) =
  Table deck (discard ++ playedCards) hands [] $ currentPlayer + 1


