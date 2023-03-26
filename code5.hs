--Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

--You may assume that each input would have exactly one solution, and you may not use the same element twice.

--You can return the answer in any order.

import qualified Data.Set as S

main :: IO ()
main = 
    print $ foo [2,7,11,15] 9

-- strategy: walkthrough the list, and build a set of tuples of the element you just saw and the element you need
-- put the tuples in a normal form, so that if you ever try to add to the set and its already there, you found your solution

foo :: [Int] -> Int -> (Int, Int)
foo xs t =
    foo' S.empty xs t

foo' :: S.Set (Int,Int) -> [Int] -> Int -> (Int, Int)
foo' s [] _ = error "bad input"
foo' s (x:xs) t = let
    tup = (min x (t-x), max x (t-x))
  in
    if tup `S.member` s 
    then tup
    else foo' (S.insert tup s) xs t