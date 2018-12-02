import Data.Array
import Data.List (nubBy)
import Control.Monad (liftM2, join)
import Control.Applicative (liftA2)
import qualified Data.MemoCombinators as Memo
import Data.Maybe (catMaybes)


-- see https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProd''' :: [a] -> [b] -> [(a, b)]
cartProd''' = liftA2 (,)

cartesianProduct a = cartProd''' a a


diffOne :: (String, String) -> Maybe String
diffOne (a,b) = if ((length a) - (length c) == 1) then (Just (map (fst) c)) else Nothing
  where c = filter(\(x,y) -> x==y) $ zip a b


main = do
  input <- readFile "input02.txt"
  let ids = lines input
  putStrLn $ show $ catMaybes $ map diffOne $ cartProd''' ids ids
