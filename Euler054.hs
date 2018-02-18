import Data.List
import Data.Ord

data Face = F2 | F3 | F4 | F5 | F6 | F7 | F8
          | F9 | FT | FJ | FQ | FK | FA | FX -- Add extra one so succ can't go out of bounds.
          deriving (Show, Eq, Ord, Read, Enum)

data Suit = D | H | S | C
  deriving (Show, Eq, Ord, Read)

data Card = Card { face :: Face,
                   suit :: Suit
                 } deriving (Eq, Ord)

instance Show Card where
  show (Card f s) = ((show f) !! 1) : show s

readCard :: String -> Card
readCard str = Card f s
  where
    f = read ('F' : [str !! 0])
    s = read ([str !! 1])

main = do
  file <- readFile "./p054_poker.txt"
  let cards = map (\g -> map readCard (words g)) (lines file)
      winners = map (\g -> compareHands (take 5 g) (drop 5 g)) cards
      ans = length $ filter (==GT) winners

  print ans


sameFace (Card f1 _) (Card f2 _) = f1 == f2
sameSuit (Card _ s1) (Card _ s2) = s1 == s2

    -- High Card: Highest value card.
    -- One Pair: Two cards of the same value.
    -- Two Pairs: Two different pairs.
    -- Three of a Kind: Three cards of the same value.
    -- Straight: All cards are consecutive values.
    -- Flush: All cards of the same suit.
    -- Full House: Three of a kind and a pair.
    -- Four of a Kind: Four cards of the same value.
    -- Straight Flush: All cards are consecutive values of same suit.
    -- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.



data HandType = HighCard
              | OnePair Face
              | TwoPairs Face Face   -- Face of each pair
              | ThreeKind Face
              | Straight
              | Flush
              | FullHouse Face Face  -- Threekind face, Pair face
              | FourKind Face
              | StraightFlush
              | RoyalFlush
              deriving (Eq, Ord, Show)

compareHands :: [Card] -> [Card] -> Ordering
compareHands h1 h2 = compare (classifyHand h1, reverse.sort $ h1)
                             (classifyHand h2, reverse.sort $ h2)

classifyHand :: [Card] -> HandType
classifyHand cs
  | highF == FA && isF && isS    = RoyalFlush
  | isF && isS                   = StraightFlush
  | (length . head) grouped == 4 = FourKind (face $ (head.head) grouped)

  | (length . head) grouped  == 3
  && (length (grouped !! 1)) == 2 = FullHouse (face . head $ grouped !! 0)
                                              (face . head $ grouped !! 1)

  | isF = Flush
  | isS = Straight

  | (length . head) grouped  == 3 = ThreeKind (face . head $ grouped !! 0)

  | (length . head) grouped  == 2
  && (length (grouped !! 1)) == 2 = TwoPairs (face . head $ grouped !! 0)
                                             (face . head $ grouped !! 1)

  | (length . head) grouped  == 2 = OnePair (face . head $ grouped !! 0)

  | (length . head) grouped == 1 = HighCard

  where
    cards = (reverse . sort) cs
    (Card highF _) = head cards
    isF = isFlush cards
    isS = isStraight cards
    grouped = reverse $ sortBy (comparing length) $ groupBy sameFace cards

isFlush :: [Card] -> Bool
isFlush (c:cs) = all (sameSuit c) cs

isStraight :: [Card] -> Bool
isStraight cs = and $ zipWith (\(Card f1 _) (Card f2 _) -> succ f1 == f2) (tail cs) cs

th1 = map readCard $ words "5H 5C 6S 7S KD"
th2 = map readCard $ words "2C 3S 8S 8D TD"
th3 = map readCard $ words "2C 3S 4S 5D 6D"

groupC cards = reverse $ sortBy (comparing length) $ groupBy sameFace cards
