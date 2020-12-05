mumu :: [Int] -> [Int] -> [[Int]]
mumu [] list = [list]
mumu sub list
    | subIndex == -1 = [list]
    | otherwise = [take subIndex list] ++ mumu sub (drop (subIndex+(length sub)) list)
    where
    subIndex = findSublist sub list 0 (length list - 1)
    findSublist sub list a b
        | length sub > (b-a+1) = -1
        | sub == (take (length sub) (drop a (take (b+1) list))) = a
        | otherwise = findSublist sub list (a+1) b

--est' li povtor elem in list
checkDuble :: (Eq a) => [a] -> Bool
checkDuble [] = False
checkDuble [x] = False
checkDuble (x:xs) = (checkTemp x xs) || checkDuble xs
    where
        checkTemp el [x] = el == x
        checkTemp el (x:xs) = (el == x) || (checkTemp el xs)

checkDuble1 :: (Eq a) => [a] -> Bool
checkDuble1 [] = False
checkDuble1 (x:xs)
    | [y | y <- xs, y == x] == [] = checkDuble1 xs
    | otherwise = True

-- vyvod vseh podspiskov
podspisok :: [a] -> [[a]]
podspisok [] = [[]]
podspisok (x:xs) = 
    let
        temp = podspisok xs
    in temp ++ map (\c -> x:c) temp

{-
podspisok :: [Int] ->[[Int]]
podspisok []  = [[]]
podspisok (x:xs) = [frstn (x:xs) n | n <- [1..length (x:xs)]] ++ podspisok xs
    where frstn x n | length x < n = []
          frstn _ 0 = []
          frstn (x:xs) n = x : frstn xs (n-1)
    -}
podspisok11:: Eq a=> [a] -> [[a]]
podspisok11 [] = [[]]
podspisok11 (x:xs) = pods [x] xs ++ podspisok11 xs
    where
        pods a [] = [a]
        pods a (y:ys) = a:pods (a++[y]) ys
