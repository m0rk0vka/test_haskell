-- ubrat' povtoryachiesya elementu

wipe :: Ord a => [a] -> [a]
wipe[] = []
wipe[x] = [x]
wipe(x:xs) = x:wipe[a|a <-xs,a /= x]

wipe1 :: Ord a => [a] -> [a]
wipe1[] = []
wipe1[x] = [x]
wipe1(x:xs) 
    | temp == [] = x:wipe1(xs)
    | otherwise = wipe1(temp1)
    where 
        temp = [a|a <- xs, a == x]
        temp1 = [a|a <- xs, a /= x]