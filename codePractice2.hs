-- Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.

-- The overall run time complexity should be O(log (m+n)).

-- strategy: find the middle of the longer array, then add elements from the shorter array until...

main :: IO ()
main = do
    print $ mySort [4,5,6,7] [1,2,3] 
    print $ medianofTwo [1,2,3] [4,5,6,7]

medianofTwo :: [Int] -> [Int] -> Int
medianofTwo xs ys =
    (mySort xs ys) !! ((length xs + length ys) `div` 2 )


mySort :: Ord a => [a] -> [a] -> [a]
mySort [] [] = []
mySort [] ys = ys
mySort xs [] = xs
mySort (x:xs) (y:ys) = 
    if x < y then x:(mySort xs (y:ys)) else y:(mySort (x:xs) ys)