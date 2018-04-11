import System.Random

data Card = Explosion | Defuse | Favor | Skip | Shuffle | Attack |
            Nope | SeeTheFuture | NoInstruction Kind
                                  deriving Show
type Kind = String

type Hand = [Card]

data Table = Table { tableDeck :: [Card], tableDiscard :: [Card], tableHands :: [Hand] }
  deriving Show

defaultDeck = unpack [(Explosion, 4), (Defuse, 6), (Favor, 4), (Skip, 4),
               (Shuffle, 4), (Attack, 4), (Nope, 4), (SeeTheFuture, 4),
               (NoInstruction "1", 4), (NoInstruction "2", 4), (NoInstruction "3", 4),
               (NoInstruction "4", 4), (NoInstruction "5", 4)]

unpack :: [(Card, Int)] -> [Card]
unpack ((card, count):list) = replicate count card ++ unpack list
unpack [] = []

shuffle :: StdGen -> [Card] -> [Card]
shuffle _ [] = []
shuffle stdGen cards = randomCard:shuffle newGen rest where
  (left, randomCard:right) = splitAt index cards
  rest = left ++ right
  (index, newGen) = randomR (0, length cards - 1) stdGen

distributeDeck :: Int -> [Card] -> Table
--distributeDeck _ [] = error "Where are my cards?"
--distributeDeck numPlayers _ | numPlayers < 2 = error "Get me players, please"
distributeDeck 0 cards = Table cards [] []
distributeDeck numPlayers cards = Table deck [] hands where
  numCards = 5
  (firstHand, restCards) = splitAt numCards cards
  restTable = distributeDeck (numPlayers - 1) restCards
  deck = tableDeck restTable
  hands = firstHand : tableHands restTable
