-- Poenyali first i last element mestami
mylo :: [a] -> [a]
mylo [] = []
mylo [x] = [x]
mylo (x:xs) = let (f,g) = swap x xs in (f:g)
    where 
        swap k [y] = (y, [k])
        swap k (y:ys) = let (n, m) = swap k ys in (n, (y:m))


-- Mult matrix
matMul :: Num a => [[a]] -> [[a]] -> [[a]]
matMul x y =
    let
        col m = [x | x:xs <- m]
        rests m = [xs | x:xs <- m,length(xs) > 0 ]
        rowMulMat r []  = []
        rowMulMat r m   = sum(zipWith (*) r (col m)):(rowMulMat r (rests m))
    in case x of
        [r]     -> [rowMulMat r y]
        (r:rs)  -> (rowMulMat r y):(matMul rs y)
