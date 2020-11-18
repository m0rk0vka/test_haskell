{-
    Split the list in half and flip each half.
-}
middleSplitThenFlip :: [a] -> [a]
middleSplitThenFlip [] = []
middleSplitThenFlip zs = list
    where
        list = (rev beg) ++ mid ++ (rev end)

        (beg, mid, end) = go zs zs
        go (x:xs) (_:_:ys) = 
            let 
                (beg, mid, end) = go xs ys 
            in 
                (x:beg, mid, end)
        go (x:xs) [_] = ([], [x], xs)
        go (x:xs) []  = ([], [], x:xs)

        rev [] = []
        rev [x] = [x]
        rev (x:xs) = rev xs ++ [x]


halve :: [a] -> ([a], [a], [a])
halve [] = ([], [], [])
halve xs = go xs xs
    where
        go (x:xs) (_:_:ys) =
            let
                (beg, mid, end) = go xs ys
            in
                (x:beg, mid, end)
        go (x:xs) [_] = ([], [x], xs)
        go xs [] = ([], [], xs) 
