{-# LANGUAGE MultiWayIf #-}

{-
Given an input string s and a pattern p, implement regular expression matching with support for '.' and '*' where:

'.' Matches any single character.
'*' Matches zero or more of the preceding element.
The matching should cover the entire input string (not partial).
-}

import Data.Char

main :: IO ()
main = do
  print $ match "aa" "a"
  print $ match "aa" "a*"
  print $ match "aa" ".*"
  print $ match "aab" ".*."
  print $ match "aabc" ".*."
  print $ match "aabc" ".*.*c"

match :: String -> String -> Bool
match xs ps = match' Nothing xs (parsePattern ps)

match' :: Maybe Char -> String -> Pattern -> Bool
match' currentC [] [] = True
match' currentC _ [] = False
match' currentC [] [p] = case p of 
  Star _ -> True
  StarDot -> True
  _ -> False
match' currentC (x:xs) (p:ps) = case p of
  CharLit c -> if c == x then match' Nothing xs ps else False
  Dot -> match' Nothing xs ps
  Star c -> if c == x then match' Nothing xs (p:ps) else False
  StarDot -> 
    case currentC of
      Nothing -> match' (Just x) xs (p:ps)
      Just c -> if c == x then match' (Just x) xs (p:ps) else match' Nothing (x:xs) ps

data Pat = CharLit Char | Dot | Star Char | StarDot
type Pattern = [Pat]

parsePattern :: String -> Pattern
parsePattern ps = parseP ps []

parseP :: String -> Pattern -> Pattern
parseP [] l = l -- pattern ends with star
parseP [p] l = if
  | p == '.'  -> l++[Dot]
  | isAlpha p -> l++[CharLit p]
  | otherwise -> error "invalid pattern"
  
parseP (p1:p2:ps) l = if 
  | p2 == '*' && p1 == '.'  -> parseP ps (l++[StarDot])
  | p2 == '*' && isAlpha p1 -> parseP ps (l++[Star p1])
  | p2 /= '*' && p1 == '.'  -> parseP (p2:ps) (l++[Dot])
  | p2 /= '*' && isAlpha p1 -> parseP (p2:ps) (l++[CharLit p1])
  | otherwise -> error "invalid pattern"
