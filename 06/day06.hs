import Data.Maybe (catMaybes)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

input = "1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9"

digit :: ReadP Char
digit =
  satisfy (\char -> char >= '0' && char <= '9')

numbers :: Int -> ReadP Int
numbers digits = do
  parse <- count digits digit
  return (read parse)

type Coordinates = (Int, Int)

parseCoordinates :: ReadP Coordinates
parseCoordinates = do
 x <- numbers 1 <|> numbers 2 <|> numbers 3
 _ <- string ", "
 y <- numbers 1 <|> numbers 2 <|> numbers 3
 return (x,y)


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    [] -> Nothing
    result -> Just (fst $ last result)

manhattanDistance :: Coordinates -> Coordinates -> Int
manhattanDistance (x1,y1) (x2,y2) = (abs (x2 -x1)) + (abs(y2 - y1))

allCoords :: String -> [Coordinates]
allCoords = catMaybes . map (parseMaybe parseCoordinates) . lines

closest p cs = fst $ minimumBy (comparing snd) $ map (\x -> (x,manhattanDistance p x)) cs

closestPoints ps p = map fst $ filter (\(x,y) -> y == (snd $ minimumBy (comparing snd) abc)) abc
  where abc = map (\x -> (x, manhattanDistance p x)) ps


boundingBox all = (left,top,right,bottom) 
  where left   = fst $ minimumBy (comparing fst) all
        top    = snd $ minimumBy (comparing snd) all
        right  = fst $ maximumBy (comparing fst) all
        bottom = snd $ maximumBy (comparing snd) all

edges (left, top, right, bottom) = [ (x,t) | x <- [l..r]  ] 
  ++ [ (x,b) | x <- [l..r]] ++ [ (l, y) | y <- [t..b]]  ++ [ (r, y) | y <- [t..b]]
  where t = top - 1
        b = bottom + 1
        l = left - 1
        r = right + 1

outerAreas all = S.fromList $ foldl (++) [] $ filter (\x -> length x == 1) $ map (\x -> closestPoints all x) $ edges $ boundingBox all

finiteAreas all = S.toList $ S.difference (S.fromList all) (outerAreas all)

withinBB (x1,y1,x2,y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x,1) | x <- xs])

bbBySize all = frequency $ map (\x -> x!!0) $ filter (\x -> x!!0 `elem` (finiteAreas all)) $ filter (\x -> length x == 1) $ map (\x -> closestPoints all x) $ withinBB $ boundingBox all

largestFiniteBBSize = snd . maximumBy (comparing snd) . bbBySize 

totalDistance p all = sum $ map (\x -> manhattanDistance x p) all

totalDistanceLT all maxDist = filter (\x -> x < maxDist) $ map(\y -> totalDistance y all) $ withinBB $ boundingBox all

main :: IO ()
main = do
  day06_input <- readFile "input06.txt" 
  let all = allCoords day06_input
  putStrLn $ "Part 1: Largest BB size: " ++ show (largestFiniteBBSize all)
  putStrLn $ "Part 2: Size where total distance < 10000: " ++ show (length (totalDistanceLT all 10000))

