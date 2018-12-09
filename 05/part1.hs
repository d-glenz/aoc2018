import Control.Monad (liftM2, ap)
import Data.Char (ord, chr)
import Data.Text (strip, unpack, pack)
import Debug.Trace (trace)

test_input1 = "aA"
test_input2 = "abBA"
test_input3 = "abAB"
test_input4 = "aabAAb"
test_input5 = "dabAcCaCBAcCcaDA"

ord_input1 = map ord test_input1
ord_input2 = map ord test_input2
ord_input3 = map ord test_input3
ord_input4 = map ord test_input4
ord_input5 = map ord test_input5

shift :: Int -> [a] -> [a]
shift l = liftM2 take length (drop l . cycle)

find_first_pos :: [Int] -> Int
find_first_pos ls 
  | null foldable = -1
  | otherwise = fst $ head foldable
  where foldable = (filter ((32 ==) . snd) . zip [1..] . map abs . ap (zipWith (-)) (shift 1)) ls

del_around :: Int -> [Int] -> [Int]
del_around n ls 
  | n == (length ls) = ls
  | otherwise        = (take (n-1) ls) ++ (drop (n+1) ls)

fold_first :: [Int] -> [Int]
fold_first = del_around =<< find_first_pos

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

iterate' :: ([Int] -> [Int]) -> [Int] -> [[Int]]
iterate' f a = trace (map (chr) a) a : iterate' f (f a)

fold_all :: [Int] -> [Int]
fold_all = converge (==) . iterate fold_first


zip' :: [a] -> [a] -> [[a]]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [x,y]:(zip' xs ys)

main :: IO ()
main = do
  day5_input <- readFile "input.txt"
  let day5_inp = unpack $ strip $ pack day5_input
      day5_ord = map (ord) $ unpack $ strip $ pack day5_input
      result = fold_all day5_ord
      pairs = zip' ['a'..'z'] ['A'..'Z']
  print $ length day5_input
  print $ length result
  print $ map (\p -> length $ fold_all $ map (ord) $ filter (\x -> not (x `elem` p)) day5_inp) pairs
