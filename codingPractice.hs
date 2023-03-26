main :: IO ()

main = 
    print $ foo [1,2,3,0,1]

type State = (Int,Int,Bool)

-- takes a list of steps and returns a possible path (sequence of step sizes)
-- strategy will be to label bad states, then select from good ones
-- if the first state is bad, then we cannot find a path
foo xs = let 
    indexs = zip xs [0,1..]
    bads = foldl (labelBad) [] (reverse indexs)
  in
    bads 

labelBad ::  [State] -> (Int,Int) -> [State]
labelBad xs x = 
    (fst x, snd x, isBadState x xs):xs
  
-- a bad state is a state where the max step size can only bring you to bad states
-- given a state, and the list of states in front of that state, are we stuck?
isBadState :: (Int,Int) -> [State] -> Bool
isBadState (v,i) [] = if v==0 then True else False
isBadState (v,i) xs =
    case v of
      0 -> True
      n -> all (==True) ((map (getLabel) (take n xs)):: [Bool])

getLabel :: State -> Bool
getLabel (_,_,l) = l

