import Data.Array
import Control.Monad (liftM2, join)
import Control.Applicative (liftA2)
import qualified Data.MemoCombinators as Memo

-- see https://swizec.com/blog/levenshtein-distance-in-haskell/swizec/4801
levenshtein::[Char] -> [Char] -> Int
levenshtein s1 s2
  | length s1 > length s2 = levenshtein s2 s1
  | length s1 < length s2 =
    let d = length s2 - length s1
    in d + levenshtein s1 (take (length s2 - d) s2)
levenshtein "" "" = 0
levenshtein s1 s2
  | last s1 == last s2 = levenshtein (init s1) (init s2)
  | otherwise = minimum [1 + levenshtein (init s1) s2,
                         1 + levenshtein s1 (init s2),
                         1 + levenshtein (init s1) (init s2)]


-- see https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/
lev :: (Eq a) => [a] -> [a] -> Int
lev [] [] = 0
lev [] ys = length ys
lev xs [] = length xs
lev (x:xs) (y:ys)
  | x == y    = lev xs ys
  | otherwise = 1 + minimum [lev xs (y:ys),
                             lev (x:xs) ys,
                             lev xs ys]


-- see https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/
memoString = Memo.list Memo.char
lev' :: String -> String -> Int
lev' xs ys = levMemo xs ys
  where levMemo = Memo.memo2 memoString memoString lev
        lev [] [] = 0
        lev [] ys = length ys
        lev xs [] = length xs
        lev (x:xs) (y:ys)
          | x == y    = levMemo xs ys
          | otherwise = 1 + minimum [levMemo xs (y:ys),
                                     levMemo (x:xs) ys,
                                     levMemo xs ys]

-- see https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/
lev'' :: (Eq a) => [a] -> [a] -> Int
lev'' xs ys = levMemo n m 
  where levMemo = Memo.memo2 (Memo.arrayRange (0,n)) (Memo.arrayRange (0,m)) lev
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo (u-1) (v-1)
          | otherwise        = 1 + minimum [levMemo u (v-1),
                                            levMemo (u-1) v,
                                            levMemo (u-1) (v-1)]


-- see https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/
lev''' :: (Eq a) => [a] -> [a] -> Int
lev''' xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 

-- see https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProd :: [a] -> [b] -> [(a, b)]
cartProd = (<*>) . fmap (,)

-- see https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProd' :: [a] -> [b] -> [(a, b)]
cartProd' xs ys = (,) <$> xs <*> ys

-- see https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProd'' :: [a] -> [b] -> [(a, b)]
cartProd'' xs ys = xs >>= \x -> ys >>= \y -> [(x, y)]

-- see https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProd''' :: [a] -> [b] -> [(a, b)]
cartProd''' = liftA2 (,)

cartesianProduct a = cartProd''' a a

idPair = head . filter (\(x,y) -> lev''' x y == 1) . join cartProd'''

day2Solution2 a = map (\(x,y) -> x) $filter (\(x,y) -> x==y) $ zip (fst a) (snd a)
-- day2Solution2' = map fst . filter (uncurry (==)) . uncurry zip
-- day2Solution2'' = map fst . filter (uncurry (==)) . uncurry zip . head . filter (uncurry (flip flip 1 . ((==) .) . lev''')) . cartesianProduct


main = do
  input <- readFile "input02.txt"
  let ids = lines input
  putStrLn $ show $ day2Solution2 $ idPair ids
