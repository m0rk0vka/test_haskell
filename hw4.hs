{-
    Calculate the determinant of matrix
-}

determinant :: [[Double]] -> Double
determinant [] = error "Empty matrix"
determinant x
    | not check = error "It's not a square matrix"
    | otherwise = det x
    where
        len = length(x)
        check = check_i 0
        check_i i = 
            let
                a 
                    | i < len - 1 = check_i (i+1)
                    | otherwise = True
            in a && (length(x!!i) == len)

        det [[a,b], [c,d]] = a*d-b*c
        det x = sum (map (\c -> ((x !! 0)!!c)*(sgn c)*(minor x c))  [0..(len-1)])

        sgn c
            | c `mod` 2 == 0 = 1.0
            | otherwise = -1.0

        minor x k = det (new (drop 1 x) k)
        new x k = map (\r -> (take (k) r)++(drop (k+1) r)) x