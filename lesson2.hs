-- hanoickaya bashnya so vtoroi na pervyi
-- show func perevoda iz Int v String
hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi from to vi 1 = ["from "++(show to)++" to "++(show from)]
hanoi from to vi n = (hanoi vi to from (n-1)) ++["from "++
    (show to)++" to "++(show from)]++ (hanoi from vi to (n-1))


-- so vtoroi na tretii
hanoi1 :: Int -> Int -> Int -> Int -> [String]
hanoi1 from to vi 1 = ["from " ++ (show to) ++ " to " ++ (show vi)]
hanoi1 from to vi n = (hanoi1 vi to from (n-1)) ++ ["from "++
    (show to)++" to "++(show vi)]++ (hanoi1 to from vi (n-1))


--qsort with filter
sort1 :: (Ord a) => [a] -> [a]
sort1 [] = []
sort1 (x:xs) = sort1 (filter (<x) xs) ++ [x] ++ sort1 (filter (>=x) xs)


--qsort with generator
mySort :: (Ord a) => [a]->[a]
mySort [] = []
mySort (x:xs) =
    let leftSort = mySort [a|a<-xs,a<=x] -- structura let in
        rightSort = mySort [a|a<-xs,a>x]
    in leftSort++[x]++rightSort


-- qsort cherez constructor spiskov i construciu gvard
sort2 :: Ord a => [a]->[a]
sort2 [] = []
sort2 [x] = [x]
sort2 (x1 : x2 : xs)
    | (x1<x2) = let (a:as) = sort2 (x1:xs) in a: sort2 (x2:as)
    | otherwise = let (a:as) = sort2 (x2:xs) in a: sort2 (x1:as)

{-- cortegi kak spiski tolko mojno hranit' elementu raznoi prirody
*Main> fst (1,2)
1
*Main> snd (1,2)
2
:t - pokazyvaet signaturu
*Main> :t zip
zip :: [a] -> [b] -> [(a, b)]
*Main> zip ['a', 'b'] [1, 2]
[('a',1),('b',2)]
}