module PackOfCards where

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq)

instance Show Suit where
  show s = case s of
    Spades -> "♠"
    Hearts -> "♥"
    Diamonds -> "♦"
    Clubs -> "♣"

instance Ord Suit where
  compare s1 s2
    | s1 == s2 = EQ
  compare Spades _ = GT
  compare Hearts s = case s of
    Spades -> LT
    otherwise -> GT
  compare Diamonds s = case s of
    Clubs -> GT
    otherwise -> LT
  compare Clubs _ = LT

data Rank = Ace | Number Int | Jack | Queen | King deriving (Eq)

instance Show Rank where
  show r = case r of
    Ace -> "A"
    (Number n) -> show n
    Jack -> "J"
    Queen -> "Q"
    King -> "K"

instance Enum Rank where
  toEnum n = case n of
    1 -> Ace
    11 -> Jack
    12 -> Queen
    13 -> King
    n -> Number n
  fromEnum r = case r of
    Ace -> 1
    (Number n) -> n
    Jack -> 11
    Queen -> 12
    King -> 13

instance Ord Rank where
  compare r1 r2
    | r1 == r2 = EQ
  compare Ace _ = LT
  compare (Number n1) (Number n2) = compare n1 n2
  compare (Number n) Ace = GT
  compare (Number n) _ = LT
  compare Jack r = case r of
    King -> LT
    Queen -> LT
    otherwise -> GT
  compare Queen r = case r of
    King -> LT
    otherwise -> GT
  compare King _ = GT

-- type Card = (Rank, Suit)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = (show r) ++ (show s)

mkCard :: String -> Card
mkCard rs = -- rank/suit
  let n = read [(head rs)] :: Int
      s = last rs
  in case head rs of
    'a' -> mkCard' Ace s
    '1' -> mkCard' Ace s
    't' -> mkCard' (Number 10) s
    'j' -> mkCard' Jack s
    'q' -> mkCard' Queen s
    'k' -> mkCard' King s
    otherwise -> mkCard' (Number n) s
  where
    mkCard' :: Rank -> Char -> Card
    mkCard' r s = case s of
      's' -> Card r Spades
      'h' -> Card r Hearts
      'd' -> Card r Diamonds
      'c' -> Card r Clubs

spades = [ Card (toEnum r) Spades | r <- [1..13] ]
hearts = [ Card (toEnum r) Hearts | r <- [1..13] ]
diamonds = [ Card (toEnum r) Diamonds | r <- [1..13] ]
clubs = [ Card (toEnum r) Clubs | r <- [1..13] ]

deck = spades ++ hearts ++ diamonds ++ clubs
