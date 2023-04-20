{- Given two strings s and t, determine if they are isomorphic.

Two strings s and t are isomorphic if the characters in s can be replaced to get t.

All occurrences of a character must be replaced with another character while preserving the order of characters. No two characters may map to the same character, but a character may map to itself.
-}

import qualified Data.Map as M

main :: IO ()
main =  do
  print $ isIso "egg" "add"
  print $ isIso "dad" "bag"
  print $ isIso "hello" "tiooq"

isIso :: String -> String -> Bool
isIso s1 s2 = 
  toNormalForm s1 == toNormalForm s2

-- egg -> [0,1,1]
-- hello -> [0,1,2,2,3]
-- dad -> [0,1,0]
toNormalForm :: String -> [Int]
toNormalForm xs =
  snd $ foldl oneChar (M.empty, []) xs

oneChar :: (M.Map Char Int, [Int]) -> Char -> (M.Map Char Int, [Int])
oneChar (m, l) c =
  case M.lookup c m of
    Just i -> (m, l++[i])
    Nothing -> (M.insert c (M.size m) m, l++[(M.size m)])
