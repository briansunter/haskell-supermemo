module SuperMemo where

-- Number of times in a row remembered.
type Repetitions = Integer

-- SuperMemo "Easiness Factor".
type Easiness = Rational

minEasiness = 1.3

-- Number of days until next review.
type Days = Int

data Interval = Interval {
 reps     :: Repetitions,
 easiness :: Easiness,
 days     :: Days
 } deriving (Eq, Show)

-- Subjective ease of remembering item.
data Quality = Blackout | Forgot | Fuzzy | Hard | Normal | Perfect
    deriving (Eq, Enum, Bounded, Show, Read)

forgot :: Quality -> Bool
forgot Blackout = True
forgot Forgot   = True
forgot Fuzzy    = True
forgot _        = False

-- Convert quality to numeric value for SuperMemo algorithm.
qualityLevel :: Quality -> Rational
qualityLevel = toRational . fromEnum

nextEasiness :: Easiness -> Quality -> Easiness
nextEasiness e q = maximum [ne, 1.3]
  where ne = e - 0.8 + 0.28 * ql - 0.02 * ql * ql
        ql = qualityLevel q

nextInterval :: Interval -> Quality -> Interval
nextInterval (Interval 1 ef _) _ = Interval 2 ef 2
nextInterval (Interval 2 ef _) _ = Interval 3 ef 6
nextInterval (Interval r ef d) q = Interval nextReps nextEase nextDays
   where nextReps = if forgot q then 1 else r+1
         nextEase = nextEasiness ef q
         nextDays = if forgot q then 1 else round $ (toRational d) * ef

-- Interval for the first review.
defaultInterval :: Interval
defaultInterval = Interval 1 2.5 1

reviews :: Interval -> [Quality] -> [Interval]
reviews i [q] = [nextInterval i q]
reviews i (q:qs) = ni:reviews ni qs
  where ni = nextInterval i q
