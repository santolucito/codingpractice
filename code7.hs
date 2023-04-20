{-# LANGUAGE MultiWayIf #-}

{- Given a sorted array of distinct integers and a target value, return the index if the target is found. If not, return the index where it would be if it were inserted in order.

You must write an algorithm with O(log n) runtime complexity.
-}

--Input: nums = [1,3,5,6], target = 5
--Output: 2

main :: IO ()
main = print $ searchInsertPos [1,3,4,6] 5

searchInsertPos :: [Int] -> Int -> Int
searchInsertPos [] t = 0
searchInsertPos xs t = let 
  mid = length xs `div` 2
  midVal = xs!!mid
 in if
  | midVal == t -> mid
--TODO there is an off-by-one error here that causes an infinite loop
  | midVal < t -> mid + searchInsertPos (drop mid xs) 5 
  | midVal > t -> searchInsertPos (take mid xs) 5
  
