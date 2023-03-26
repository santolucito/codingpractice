--You have a long flowerbed in which some of the plots are planted, and some are not. However, flowers cannot be planted in adjacent plots.

--Given an integer array flowerbed containing 0's and 1's, where 0 means empty and 1 means not empty, and an integer n, return if n new flowers can be planted in the flowerbed without violating the no-adjacent-flowers rule.

main :: IO ()
main = do
    print $ adj [1,0,0,0,0,0,1] 1
    print $ adj [1,0,0,1] 2
    print $ adj [1,0,0,0] 2

foo :: (Int, [Int]) -> Int -> (Int, [Int])
foo (acc, slidingWin) nextElem = let
    xs = slidingWin
    nextWin = tail xs++[nextElem]
  in
    (acc+fromEnum(nextWin==[0,0,0]), 
    if nextWin==[0,0,0] then [0,1,0] else nextWin)

adj :: [Int] -> Int -> Int
adj xs n = let
    numSpaces = fst $ foldl foo (0,[0,0,0]) (xs++[0])
  in
    numSpaces



