import Data.List
import Data.List.Split
import System.Random
import System.Random.Shuffle
import PackOfCards

type Hand = [Card]
type Points = Float
type Points' = ([[Card]], Points)
data Crib = Own | Opp

randomDeal :: Int -> (Hand, Card)
randomDeal seed = (h, s) where
  h = take 6 $ shuffle' deck 52 gen
  s = head . drop 6 $ shuffle' deck 52 gen
  gen = mkStdGen seed

mkHand :: String -> Hand
mkHand = map mkCard . chunksOf 2

valid :: Hand -> Bool
valid h = length h `elem` [4,5] && noRepeats h

value :: Card -> Points
value c = case rank c of
  Ace -> 1
  Number n -> fromIntegral n
  otherwise -> 10

handTotal :: Hand -> Points
handTotal = sum . map value

-- countBy :: (Eq b) => (a -> b) -> b -> [a] -> Int
-- countBy f x = length . filter (==x) . map f

-- count :: Rank -> Hand -> Int
-- count = countBy rank

sorted :: (Ord a) => [a] -> Bool
sorted xs = xs == sort xs

noRepeats :: (Eq a) => [a] -> Bool
noRepeats xs = (length $ nub xs) == length xs

sameBy :: (Eq b) => (a -> b) -> [a] -> Bool
sameBy f = (==1) . length . nub . map f

longest :: [[a]] -> [[a]]
longest xs = filter ((==n) . length) xs where
  n = maximum . map length $ xs

pairs :: Hand -> [[Card]]
pairs h = filter ((==2) . length) $ allPairs h

allPairs :: Hand -> [[Card]]
allPairs h = [ cs | cs <- subsequences h
                  , length cs > 1
                  , sameBy rank cs ]

maxPairs :: Hand -> [[Card]]
maxPairs = longest . allPairs

pairScore :: Hand -> Points
pairScore = fromIntegral . (*2) . length . pairs

fifteens :: Hand -> [[Card]]
fifteens h = [ cs | cs <- subsequences h
                  , handTotal cs == 15 ]

fifteenScore :: Hand -> Points
fifteenScore = fromIntegral . (*2) . length . fifteens

allRuns :: Hand -> [[Card]]
allRuns h = map sort rs where
  rs = [ cs | cs <- subsequences h
            , length cs > 2
            , noRepeats . map rank $ cs
            , length cs == (fromEnum . rank $ maximum cs) - (fromEnum . rank $ minimum cs) + 1 ]

maxRuns :: Hand -> [[Card]]
maxRuns = longest . allRuns

runScore :: Hand -> Points
runScore h
  | maxRuns h == [] = 0
  | otherwise = fromIntegral $ (length . head $ maxRuns h) * (length $ maxRuns h)

isFlush :: Hand -> Bool
isFlush = sameBy suit

flushScore :: Hand -> Points
flushScore h
  | isFlush h && length h `elem` [4,5] = fromIntegral $ length h
  | otherwise = 0

nobs :: Hand -> Card -> Bool
nobs h starter = (Card Jack s) `elem` h
  where s = suit starter

nobsScore :: Hand -> Card -> Points
nobsScore h starter = if nobs h starter then 1 else 0

-- data Score = Score [Points] [[[Card]]] deriving (Show)

data Score = Score {
    pairPts :: Points
  , pairCards :: [[Card]]
  , fifteenPts :: Points
  , fifteenCards :: [[Card]]
  , runPts :: Points
  , runCards :: [[Card]]
  , nobsPts :: Points
  , flushPts :: Points
}

fullScore :: Hand -> Card -> Score
fullScore h s = let
  h' = s : h
  pairPts = pairScore h'
  pairCards = maxPairs h'
  fifteenPts = fifteenScore h'
  fifteenCards = fifteens h'
  runPts = runScore h'
  runCards = maxRuns h'
  nobsPts = nobsScore h s
  flushPts = if isFlush h' then flushScore h' else flushScore h
  -- in Score [pairPts, fifteenPts, runPts, nobsPts, flushPts] [pairCards, fifteenCards, runCards]
  in Score pairPts pairCards fifteenPts fifteenCards runPts runCards nobsPts flushPts

instance Show Score where
  show score =
    "Pairs: "    ++ (show $ pairCards score)    ++ " --> " ++ (show $ pairPts score) ++ "\n" ++
    "Fifteens: " ++ (show $ fifteenCards score) ++ " --> " ++ (show $ fifteenPts score) ++ "\n" ++
    "Runs: "     ++ (show $ runCards score)     ++ " --> " ++ (show $ runPts score) ++ "\n" ++
    flushMessage ++ nobsMessage ++
    "Total: " ++ (show $ scoreSum score)
      where
        flushMessage = if flushPts score > 0 then (show $ flushPts score) ++ " for a flush\n" else ""
        nobsMessage = if nobsPts score  == 1 then "...and 1 for his nob\n" else ""

scoreSum :: Score -> Points
scoreSum (Score p _ f _ r _ n fl) = p + f + r + n + fl

handScore :: Hand -> Card -> Points
-- score h s = scoreSum $ itemisedScore h s
handScore h = scoreSum . fullScore h

combinedScore :: Hand -> [Card] -> Crib -> Card -> Points
combinedScore h d c s = case c of
  Own -> handScore h s + handScore d s
  Opp -> handScore h s - handScore d s

fst' :: (a,b,c,d) -> a
fst' (w,x,y,z) = w

snd' :: (a,b,c,d) -> b
snd' (w,x,y,z) = x

avgScore :: Hand -> [Card] -> Crib -> Points
avgScore h d c = s / l where
  s = sum . map snd $ starterScores h d c
  l = fromIntegral $ 52 - length h - length d

minScore :: Hand -> [Card] -> Crib -> Points
minScore h d c = snd . last $ starterScores h d c

maxScore :: Hand -> [Card] -> Crib -> Points
maxScore h d c = snd . head $ starterScores h d c

starterScores :: Hand -> [Card] -> Crib -> [(Card, Points)]
starterScores h d c = reverse . sortOn snd . map starterWithScore $ reducedDeck where
  starterWithScore s = (s, combinedScore h d c s)
  reducedDeck = deck \\ (h ++ d)

discardScores :: Hand -> Crib -> [([Card], Points, Points, Points)]
discardScores h c = reverse . sortOn snd' . map discardWithScore $ discards where
  discardWithScore d = (d, avgScore (h \\ d) d c, minScore (h \\ d) d c, maxScore (h \\ d) d c)
  discards = filter ((==2) . length) $ subsequences h

bestDiscard :: Hand -> Crib -> [Card]
bestDiscard h c = fst' . head $ discardScores h c
