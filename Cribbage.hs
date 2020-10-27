import Data.List
import Data.List.Split
import System.Random
import System.Random.Shuffle
-- import MyTest
import PackOfCards

type Hand = [Card]
type Points = Float
type Points' = ([[Card]], Points)

-- getRandomHand :: Int -> Int -> Hand
-- getRandomHand n seed = let gen = mkStdGen seed in take n $ shuffle' deck 52 gen
--
-- getRandomStarter :: Int -> Card
-- getRandomStarter seed = let gen = mkStdGen seed in head $ shuffle' deck 52 gen

randomDeal :: Int -> (Hand, Card)
randomDeal seed = (h, s) where
  h = take 6 $ shuffle' deck 52 gen
  s = head . drop 6 $ shuffle' deck 52 gen
  gen = mkStdGen seed

mkHand :: String -> Hand
mkHand = map mkCard . chunksOf 2

valid :: Hand -> Bool
valid h = length h `elem` [4,5] && noRepeats h


-- count :: Rank -> Hand -> Int
-- -- count r = length . filter (==r) . map rank
-- count = countBy rank
--
-- countBy :: (Eq b) => (a -> b) -> b -> [a] -> Int
-- countBy f x = length . filter (==x) . map f

sorted :: (Ord a) => [a] -> Bool
sorted xs = xs == sort xs

noRepeats :: (Eq a) => [a] -> Bool
noRepeats xs = (length $ nub xs) == length xs

sameBy :: (Eq b) => (a -> b) -> [a] -> Bool
sameBy f = (==1) . length . nub . map f

pairs :: Hand -> [[Card]]
-- pairs h = [ [c1,c2] | c1 <- h, c2 <- h
--                         , rank c1 == rank c2
--                         , c1 < c2 ]
pairs h = filter ((==2) . length) $ allPairs h

allPairs :: Hand -> [[Card]]
allPairs h = [ cs | cs <- subsequences h
                  , length cs > 1
                  , sameBy rank cs ]

maxPairs :: Hand -> [[Card]]
maxPairs h = filter ((==n) . length) $ allPairs h
  where
    n = maximum . map length . allPairs $ h

pairScore :: Hand -> Points
pairScore = fromIntegral . (*2) . length . pairs

-- pairs :: Hand -> [[Card]]
-- pairs h = filter ((==2) . length) $ allPairs h
--
-- royalPairs :: Hand -> [[Card]]
-- royalPairs h = filter ((==3) . length) $ allPairs h
--
-- doubleRoyalPairs :: Hand -> [[Card]]
-- doubleRoyalPairs h = filter ((==4) . length) $ allPairs h

fifteens :: Hand -> [[Card]]
fifteens h = [ cs | cs <- subsequences h
                  , handTotal cs == 15 ]
                  -- , noRepeats cs ]

fifteenScore :: Hand -> Points
fifteenScore = fromIntegral . (*2) . length . fifteens

allRuns :: Hand -> [[Card]]
allRuns h = map sort rs
  where
    rs = [ cs | cs <- subsequences h
              , length cs > 2
              , noRepeats . map rank $ cs
              , length cs == (fromEnum . rank $ maximum cs) - (fromEnum . rank $ minimum cs) + 1 ]

maxRuns :: Hand -> [[Card]]
maxRuns h = filter ((==n) . length) $ allRuns h
  where
    n = maximum . map length . allRuns $ h

runScore :: Hand -> Points
runScore h = case maxRuns h of
  [] -> 0
  otherwise -> fromIntegral $ (length . head $ maxRuns h) * (length $ maxRuns h)

isFlush :: Hand -> Bool
isFlush = sameBy suit
-- isFlush h = (==1) $ length . nub . map suit $ h

flushScore :: Hand -> Points
flushScore h
  | isFlush h && length h `elem` [4,5] = fromIntegral $ length h
  | otherwise = 0

nobs :: Hand -> Card -> Bool
nobs h starter = (Card Jack s) `elem` h
  where s = suit starter
-- nobs [] _ = False
-- nobs (Card Jack s:_) starter
--   | suit starter == s = True
--   | otherwise = False
-- nobs (_:xs) starter = nobs xs starter

nobsScore :: Hand -> Card -> Points
-- nobsScore _ (Card (Number 0) Spades) = 0
nobsScore h starter = if nobs h starter then 1 else 0

data Score = Score { ps :: Points, fs :: Points, rs :: Points, nbs :: Points, fl :: Points }

instance Show Score where
  show score = -- (Score ps fs rs nbs fl)
    "Pairs: " ++ (show $ ps score) ++
    "\nFifteens: " ++ (show $ fs score) ++
    "\nRuns: " ++ (show $ rs score) ++
    flushMessage ++ nobMessage ++
    "\nTotal: " ++ show (scoreSum score)
      where
        nobMessage = if nbs score  == 1 then "\nand 1 for his nob" else ""
        flushMessage = if fl score > 0 then "\nand " ++ (show $ fl score) ++ " for a flush" else ""

data ScoreCards = ScoreCards { pcs :: [[Card]], fcs :: [[Card]], rcs :: [[Card]] }

instance Show ScoreCards where
  show score =
    "Pairs: " ++ (show $ pcs score) ++
    "\nFifteens: " ++ (show $ fcs score) ++
    "\nRuns: " ++ (show $ rcs score)
    -- ". Total: " ++ show (scoreSum score)

scoreSum :: Score -> Points
scoreSum (Score ps fs rs nbs fl) = ps + fs + rs + nbs + fl

value :: Card -> Points
value c = case rank c of
  Ace -> 1
  Number n -> fromIntegral n
  otherwise -> 10

handTotal :: Hand -> Points
handTotal = sum . map value

score' :: Hand -> Points
score' h = score h (Card (Number 100) Spades)

score :: Hand -> Card -> Points
score h s = scoreSum $ itemisedScore h s

combinedScore :: Hand -> [Card] -> Bool -> Card -> Points
combinedScore h d myCrib s
  | myCrib = score h s + score d s
  | otherwise = score h s - score d s

itemisedScore :: Hand -> Card -> Score
itemisedScore h s = let
  h' = s : h
  p = pairScore h'
  f = fifteenScore h'
  r = runScore h'
  n = nobsScore h s
  f' = if isFlush h' then flushScore h' else flushScore h
  in Score p f r n f'

explainScore :: Hand -> Card -> ScoreCards
explainScore h s = let
  h' = s : h
  p = maxPairs h'
  f = fifteens h'
  r = maxRuns h'
  in ScoreCards p f r

avgScore :: Hand -> Points
avgScore h = s / l where
  s = sum scores
  l = fromIntegral $ length scores
  scores = map (score h) (deck \\ h)

avgScoreWithCrib :: Hand -> [Card] -> Bool -> Points
avgScoreWithCrib h d myCrib = s / l where
  s = sum scores
  l = fromIntegral $ length scores
  scores = map (combinedScore h d myCrib) reducedDeck
  reducedDeck = deck \\ (h ++ d)

-- discard :: Hand -> Hand -> Hand
-- discard = \\

starterScores :: Hand -> [(Card, Points)]
starterScores h = reverse . sortOn snd . map (\s -> (s, score h s)) $ (deck \\ h)

starterScoresWithCrib :: Hand -> [Card] -> Bool -> [(Card, Points)]
starterScoresWithCrib h d myCrib = reverse . sortOn snd . map cribScore $ reducedDeck where
  cribScore s = (s, combinedScore h d myCrib s)
  reducedDeck = deck \\ (h ++ d)

discardScores :: Hand -> [([Card], Float)]
discardScores h = reverse . sortOn snd . map (\d -> (d, avgScore $ h \\ d)) $ discards where
  discards = filter ((==2) . length) $ subsequences h

discardScoresWithCrib :: Hand -> Bool -> [([Card], Float)]
discardScoresWithCrib h myCrib = reverse . sortOn snd . map discardScore $ discards where
  discardScore d = (d, avgScoreWithCrib (h \\ d) d myCrib)
  discards = filter ((==2) . length) $ subsequences h

bestDiscard :: Hand -> [Card]
bestDiscard = fst . head . discardScores

bestDiscardWithCrib :: Hand -> Bool -> [Card]
bestDiscardWithCrib h myCrib = fst . head $ discardScoresWithCrib h myCrib
