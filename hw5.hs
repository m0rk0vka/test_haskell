{--
-- Search sublist from the list and
-- dividing list to the parts on sublist
--}

subCut :: (Eq a) => [a] -> [a] -> Bool{-[[a]]-}
subCut x y = check x y
    where
        check x y
            | length(x) > length(y) = error "Sublist longer than list"
            | otherwise = isPrefix x y {-search x y-}

        {-search xs (y:ys) 
            | not isPrefix xs (y:ys) = [y]:isPrefix xs ys
            | otherwise = isPrefix xs (drop length(xs) (y:ys))-}

        isPrefix [] _ = True
        isPrefix _ [] = False
        isPrefix (x:xs) (y:ys)
            | x == y = isPrefix xs ys
            | otherwise = False
