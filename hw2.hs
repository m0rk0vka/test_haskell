-- soqtirovka puzirkom

bubbleSort1 :: (Ord a) => [a] -> [a]
bubbleSort1 [x] = [x]
bubbleSort1 (x:y:xs)
    | x > y = y : bubbleSort1 (x:xs)
    | otherwise = x : bubbleSort1 (y:xs)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort list
    {-let temp = bubbleSort1 list
    in bubbleSort (init temp) ++ [last temp]-}
    | list == bubbleSort1 list = list
    | otherwise = bubbleSort (bubbleSort1 list)

