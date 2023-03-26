--You are given an array of k linked-lists lists, each linked-list is sorted in ascending order.

--Merge all the linked-lists into one sorted linked-list and return it.

--assume we drop dups

main :: IO ()
main =
    print $ merged [[1,2,3], [4,5,6], [1,3,4,7], []]

merged :: Ord a => [[a]] -> [a]
merged [] = []
merged xs = let
    ys = (filter (/= []) xs)
    listOfMins = map head ys
  in
    case listOfMins of 
        [] -> []
        ms -> minimum listOfMins: merged (removeOneMin (minimum ms) ys)

removeOneMin :: Ord a => a -> [[a]] -> [[a]]
removeOneMin m xs =
    map (\x -> if head x == m then tail x else x) xs