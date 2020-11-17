-- razbit' massiv popolam i perevernut' polovinki
{-middleSplitThenSort :: (Ord a) => [a] -> [a]
middleSplitThenSort [] = []
middleSplitThenSort [x] = [x]
middleSplitThenSort [x,y] = [y, x]
middleSplitThenSort (x:xs) =
    let
        (f, g) = del f g
    in
        (xs, [])
    where 
        del f g 
            | length f == length g = (f, g)
            | length f > length g = ((init f), (last f) ++ g)-}

halve :: [a] -> ([a], [a], [a])
halve [] = ([],[],[])
halve xs = go xs xs
  where
    go (x:xs) (_:_:ys) = let (first, mid, last) = go xs ys in (x:first, mid,  last)
    go (x:xs) [_] = ([], [x], xs)
    go (x:xs) []  = ([], [], x:xs)