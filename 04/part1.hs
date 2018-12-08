import Control.Applicative ((<|>))
import Data.List.Split (splitOneOf)
import Data.List (intercalate, mapAccumL, maximumBy)
import Data.Maybe (catMaybes, fromJust)
import Text.ParserCombinators.ReadP
import Data.Ord (comparing, Ordering(LT), Ordering(GT))
import Data.Sort (sortBy)
import Data.Function (on)
import Data.Map (fromListWith, toList)
import Text.Printf (printf)


entries = "[1518-11-01 00:00] Guard #10 begins shift\n\
\[1518-11-01 00:05] falls asleep\n\
\[1518-11-01 00:25] wakes up\n\
\[1518-11-01 00:30] falls asleep\n\
\[1518-11-01 00:55] wakes up\n\
\[1518-11-01 23:58] Guard #99 begins shift\n\
\[1518-11-02 00:40] falls asleep\n\
\[1518-11-02 00:50] wakes up\n\
\[1518-11-03 00:05] Guard #10 begins shift\n\
\[1518-11-03 00:24] falls asleep\n\
\[1518-11-03 00:29] wakes up\n\
\[1518-11-04 00:02] Guard #99 begins shift\n\
\[1518-11-04 00:36] falls asleep\n\
\[1518-11-04 00:46] wakes up\n\
\[1518-11-05 00:03] Guard #99 begins shift\n\
\[1518-11-05 00:45] falls asleep\n\
\[1518-11-05 00:55] wakes up"

--------------------------
--      PARSING         --
--------------------------

digit :: ReadP Char
digit =
  satisfy (\char -> char >= '0' && char <= '9')

numbers :: Int -> ReadP Int
numbers digits = do
  parse <- count digits digit
  return (read parse)

type Datetime = (Int, Int, Int, Int, Int)

-- validation after ReadP
timestamp :: ReadP Datetime
timestamp = do
  year   <- numbers 4
  _      <- string "-"
  month  <- numbers 2
  _      <- string "-"
  day    <- numbers 2
  _      <- string " "
  hour   <- numbers 2
  _      <- string ":"
  minute <- numbers 2
  if year < 0 || year > 2018 || month < 1 || month > 12 || day < 1 || day > 31 || hour > 23 || hour < 0 || minute < 0 || minute > 59 then
      pfail
  else
      return (year, month, day, hour, minute)

type Guard = Int

-- OR parsing
guardNumber :: ReadP Guard
guardNumber = do
  string "Guard #"
  guard <- numbers 2 <|> numbers 3 <|> numbers 4
  string " "
  return guard

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    [] -> Nothing
    result -> Just (fst $ last result)

data Action= StartingToWork | FallingAsleep | WakingUp deriving (Show, Eq)

parseAction :: String -> Action
parseAction action
  | action == "begins shift" = StartingToWork
  | action == "falls asleep" = FallingAsleep
  | otherwise                = WakingUp

data TempEntry = TempEntry { month'::Int, day'::Int, minute':: Int, guard':: Maybe Guard, action':: Action} deriving Show
data Entry     = Entry     { month ::Int, day ::Int, minute :: Int, guard :: Guard,       action :: Action} deriving Show

-- TODO: Improve 'let action = parseAction tmp_action' bit?
-- Maybe-parsing with fmap
parseLogEntry :: ReadP TempEntry
parseLogEntry = do
  string "["
  (_, month, day, hour, minute) <- timestamp
  string "] "
  guard <- option Nothing (fmap Just guardNumber)
  tmp_action <- many1 (satisfy (\char -> (char >= 'a' && char <= 'z') || char == ' '))
  let action = parseAction tmp_action
  if hour == 23 then
    return (TempEntry {month'=month, day'=(day+1), minute'=0, guard'=guard, action'=action})
  else
    return (TempEntry {month'=month, day'=day, minute'=minute, guard'=guard, action'=action})

--------------------------
--    COMPARING         --
--------------------------

-- monoids for comparison
compareTempEntry :: Maybe TempEntry -> Maybe TempEntry -> Ordering
compareTempEntry Nothing  _        = LT
compareTempEntry _        Nothing  = GT
compareTempEntry (Just x) (Just y) = comparing month' x y `mappend` comparing day' x y `mappend` comparing minute' x y 


--------------------------
--     MAP_ACCUM_L      --
--------------------------

-- complicated mapAccumL logic
fixGuardInEntry :: Int -> Maybe TempEntry -> (Int, Maybe Entry)
fixGuardInEntry oldGuard Nothing = (oldGuard, Nothing)
fixGuardInEntry oldGuard (Just entry)
  | guard' entry == Nothing = (oldGuard, Just Entry {month = (month' entry),
                                                     day   = (day' entry),
                                                     minute= (minute' entry),
                                                     guard = oldGuard,
                                                     action= (action' entry)})
  | otherwise = (justGuard, Just Entry {month = (month' entry),
                                        day   = (day' entry),
                                        minute= (minute' entry),
                                        guard = justGuard,
                                        action= (action' entry)})
    where justGuard = fromJust (guard' entry)


parseAllEntries :: [String] -> [Maybe TempEntry]
parseAllEntries = map (parseMaybe parseLogEntry)

sortAndAugmentEntries :: [Maybe TempEntry] -> [Entry]
sortAndAugmentEntries = catMaybes . snd . mapAccumL fixGuardInEntry 0 . sortBy compareTempEntry

getCleanData :: String -> [Entry]
getCleanData = filter (\e -> (action e) /= StartingToWork) .sortAndAugmentEntries . parseAllEntries . lines


fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l ):(group n (drop n l))
  | otherwise = error "Non-positive n"


asleepIntervalsByGuard' :: String -> [(Guard,Int,[Int])]
asleepIntervalsByGuard' = map (\(x:y:[]) -> (guard x, ((minute y)-1) - (minute x),[(minute x),((minute y)-1)])) . group 2 . getCleanData

filterByKey  tl k = filter (\(x,y) -> x == k) tl
filterOutKey tl k = filter (\(x,y) -> x /= k) tl

filterByKey3  tl k = filter (\(x, _, _) -> x == k) tl
filterOutKey3 tl k = filter (\(x, _, _) -> x /= k) tl

aggregateLookup :: (Int -> Int -> Int) -> Int -> [(Guard,Int)] -> [(Guard,Int)]
aggregateLookup f _ [] = []
aggregateLookup f _ (tuple:[]) = [tuple]
aggregateLookup f i tupleList
  | null (filterOutKey tupleList firstKey) = (firstKey, foldedValues):[]
  | otherwise = (aggregateLookup f i (filterOutKey tupleList firstKey)) ++ [(firstKey, foldedValues)]
    where
      firstKey = fst $ tupleList!!0
      foldedValues = foldl f i $ map (snd) $ filterByKey tupleList firstKey

aggregateLookup' :: (Int -> Int -> Int) -> Int -> [(Guard,Int,[Int])] -> [(Guard,Int,[Int])]
aggregateLookup' f _ [] = []
aggregateLookup' f _ (tuple:[]) = [tuple]
aggregateLookup' f i tupleList
  | null (filterOutKey3 tupleList firstKey) = (firstKey, foldedValues, []):[]
  | otherwise = (aggregateLookup' f i (filterOutKey3 tupleList firstKey)) ++ [(firstKey, foldedValues, [])]
    where
      firstKey = fst3 $ tupleList!!0
      foldedValues = foldl f i $ map (snd3) $ filterByKey3 tupleList firstKey

aggregateGuardMinute = aggregateLookup (+) 0 
aggregateGuardMinute' = aggregateLookup' (+) 0 


main :: IO ()
main = do 
  day4_input <- readFile "input04.txt"
  let intvls = asleepIntervalsByGuard' day4_input
      part1_1 = fst3 $ maximumBy (comparing snd3) $ aggregateGuardMinute' intvls
      highestIntervals = filterByKey3 intvls part1_1
      allMinutes = foldl (++) [] $ map (\(x:y:[]) -> [x..y]) $ map (thd3) $ highestIntervals
      minutesByFreq = toList (fromListWith (+) [(x,1::Int) | x <- allMinutes ])
      part1_2 = maximumBy (comparing snd) minutesByFreq
      part2 = maximumBy (comparing thd3) $ map (\(g,arr) -> (g,fst $ maximumBy (comparing snd) arr,snd $ maximumBy (comparing snd) arr)) $ map (\(g,ms) -> (g,toList $ fromListWith (+) [(x,1::Int) | x <- ms ])) $ toList $ fromListWith (++) $ map (\(g,d,(s:e:[])) -> (g,[s..e])) $ intvls
  printf "Min. %d: Guard #%d * %d = %d\n" (snd part1_2) part1_1 (fst part1_2) (part1_1 * (fst part1_2)) 
  printf "ID: %d, Minute: %d, Freq.: %d - ID*Min= %d\n" (fst3 part2) (snd3 part2) (thd3 part2) ((fst3 part2) * (snd3 part2))



