{--
-- Search sublist from the list and
-- dividing list to the parts on sublist
--}

-- Delete sublist from the list
subCut :: (Eq a) => [a] -> [a] -> {-Bool-}[a]
subCut x y = check x y
    where
        check x y
            | length(x) > length(y) = error "Sublist longer than list"
            | otherwise = search x y

        search xs (y:ys)
            | length(y:ys) < length(xs) = (y:ys) 
            | not (isPrefix xs (y:ys)) = [y]++(search xs ys)
            | otherwise = search xs (drop (length(xs)) (y:ys))
        search xs [] = []

        isPrefix [] _ = True
        isPrefix _ [] = False
        isPrefix (x:xs) (y:ys)
            | x == y = isPrefix xs ys
            | otherwise = False

subCutAndSplit :: (Eq a) => [a] -> [a] -> [[a]]
subCutAndSplit x y = check x y
    where
        check x y
            | length(x) == 0 = error "Sublist is empty"
            | length (x) > length (y) = error "Sublist longer than list"
            | otherwise = tempSplit x y

        tempSplit _ [] = []
        tempSplit xs (y:ys) 
            | length(xs) > length(y:ys) = [(y:ys)]
            | isPrefix xs (y:ys) = [] : (tempSplit xs (drop (length (xs)) (y : ys)))
            | otherwise = 
                let 
                    list = ([y] ++ (tempSplit2 xs ys))
                in list:(tempSplit xs (drop (length(list)) (y:ys)))

        tempSplit2 _ [] = []
        tempSplit2 xs (y:ys)
            | isPrefix xs (y:ys) = []
            | otherwise = [y]++(tempSplit2 xs ys)

        isPrefix [] _ = True
        isPrefix _ [] = False
        isPrefix (x : xs) (y : ys)
            | x == y = isPrefix xs ys
            | otherwise = False
