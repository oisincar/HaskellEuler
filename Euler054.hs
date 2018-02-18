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
  -- | isF && isS && highF == FA      = RoyalFlush
  | isF && isS                     = StraightFlush
  | bigGSize == 4                  = FourKind bigGFace
  | bigGSize == 3 && sndgsize == 2 = FullHouse bigGFace sndGFace
  | isF                            = Flush
  | isS                            = Straight
  | bigGSize == 3                  = ThreeKind bigGFace
  | bigGSize == 2 && sndgsize == 2 = TwoPairs bigGFace sndGFace
  | bigGSize == 2                  = OnePair bigGFace
  | bigGSize == 1                  = HighCard

  where
    cards = (reverse . sort) cs
    -- (Card highF _) = head cards
    isF = all (sameSuit $ head cs) $ tail cs
    isS = and $ zipWith (\c1 c2 -> (succ . face) c1 == face c2) (tail cards) cards
    grouped = reverse $ sortBy (comparing length) $ groupBy sameFace cards

    bigGSize = length (grouped !! 0) -- big group size
    bigGFace = face . head $ (grouped !! 0)
    sndgsize = length (grouped !! 1) -- small group size
    sndGFace = face . head $ (grouped !! 1)
