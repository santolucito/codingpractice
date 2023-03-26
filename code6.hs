{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Given an array of points where points[i] = [xi, yi] represents a point on the X-Y plane, return the maximum number of points that lie on the same straight line.


import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import GHC.Float
import Data.Hashable
import GHC.Generics

main :: IO ()
main = do
    print $ foo $ toTups [[1,1],[2,2],[3,3]]
    print $ foo $ toTups [[1,1],[3,2],[5,3],[4,1],[2,3]]


toTups = map (\x -> (x!!0,x!!1))


type Point = (Int,Int)
getX :: Point -> Int
getX = fst
getY :: Point -> Int
getY = snd

data Formula = Formula {m :: Double, b :: Double} deriving (Generic, Eq, Hashable, Show) -- y = mx + b (need to deal with case where line is vertical (x = \infin))

inLine :: Formula -> Point -> Bool
inLine f (x,y) = (int2Double y) == (m f) * int2Double x + (b f)

formLine :: Point -> Point -> Formula
formLine p1'@(x1,y1) p2'@(x2,_) = let
    (p1, p2) = if 
                | x1 < x2 -> (p1', p2')
                | x1 > x2 -> (p2', p1')
                | x1 == x2 -> undefined -- TODO???
    slope = (int2Double (getY p1) - int2Double (getY p2)) / (int2Double (getX p1) - int2Double (getX p2))
    intercept = int2Double y1 - slope * int2Double x1 -- y - m*x = b
  in      
    Formula {m = slope, b = intercept} 
-- strategy: create a map of (HashMap Formula (Set Points)), then take the size of the largest set
-- for each point, find the formula required to form a line with every other point
-- if the set already exists for that formula add the point to the set otherwise create a new set
foo :: [Point] -> _
foo xs = 
    maxSize $ foo' (H.empty) (xs)

maxSize :: H.HashMap a (S.Set Point) -> _
maxSize m = maximum $ fmap S.size m
    -- S.size. maximum

foo' :: H.HashMap Formula (S.Set Point) -> [Point] -> H.HashMap Formula (S.Set Point)
foo' m [] = m
foo' m (x:xs) = let
    fs = map (\x'-> (formLine x x', x')) xs
    m' = foldl (\m (f,x') -> H.insertWith S.union f (S.fromList [x,x']) m) m fs
 in
    foo' m' xs
