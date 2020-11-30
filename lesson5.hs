detMat::Num a=> [[a]]-> a
detMat [] = error "skip"
detMat [[x]] = x
detMat [[a,b],[c,d]] = a * d - c * b
detMat ([]:xss) = 0
detMat ((x:xs):xss) = x * (-1)^(length(xss) - length(xs) + 2) * detMat[delN (length(xss) - length(xs)) y | y <- xss] + detMat((xs):xss)
    where
        delN i (y:ys)
            | i == 0    = ys
            | otherwise = y:delN (i-1) ys


inverseMat :: [[Float]] -> [[Float]]
inverseMat x 
    | not check = error "It's not a square matrix"
    | count_det == 0 = error "The determinant is 0. The system has an infinite set of solutions."
    | otherwise = mul x 0
    where
        len = length(x)
        count_det = det x
        check = check_i 0
        check_i i = 
            let
                a 
                    | i < len - 1 = check_i (i+1)
                    | otherwise = True
            in a && (length(x!!i) == len)

        mul x i
            | i < length(x)-1 = (map (\c -> c  / count_det) ((attach (trans x))!!i)):(mul x (i+1))
            | otherwise = [map (\c -> c / count_det) ((attach (trans x))!!i)]

        det [[x]] = x
        det [[a,b],[c,d]] = a * d - c * b
        det ([]:xss) = 0
        det ((x:xs):xss) = x * (-1)^(length(xss) - length(xs) 
                + 2) * detMat[delN (length(xss) - length(xs)) y | y <- xss] + detMat((xs):xss)

        trans x = foldr1 (zipWith (++)) (map ( map (\ y -> [y]) ) x)

        attach x = attach1 x 0
        attach1 x i 
            | i < len - 1 = (attach2 x i 0):(attach1 x (i+1))
            | otherwise = [attach2 x i 0]
        attach2 x i j
            | j < length(x!!i) - 1 = (((-1)^(i + j + 2) * det[delN j y | y <- (delNN i x)]):(attach2 x i (j+1)))
            | otherwise = [(-1)^(i + j + 2) * det[delN j y | y <- (delNN i x)]]
            
        delN i (y:ys)
            | i == 0    = ys
            | otherwise = y:delN (i-1) ys

        delNN i ((y:ys):yss)
            | i == 0 = yss
            | otherwise = (y:ys):(delNN (i-1) yss)


-- Формирует строку транспонированной матрицы
frstrow :: [[Float]] -> [Float] 
frstrow [] = []
frstrow (x:xs) = head x : frstrow xs

--Вспомогательная для вычеркивания столбца (удаляет выбранный элемент из строки)
minstolbvs :: [Float] -> Int -> [Float] 
minstolbvs (x:xs) y = if y==1 then xs else x : minstolbvs xs (y-1)

-- Вычеркивание выбранного столбца
minstolb :: [[Float]] -> Int -> [[Float]] 
minstolb [] _ = []
minstolb (x:xs) y = minstolbvs x y : minstolb xs y

-- Основная функция транспанирования
transp :: [[Float]] -> [[Float]] 
transp [[]] = [[]] 
transp (x:xs) = if (tail x == []) then [frstrow (x:xs)]
else  frstrow (x:xs) : transp(minstolb (x:xs) 1)


--Убирает выбранную строку
minrow :: [[Float]] -> Int -> [[Float]]
minrow (x:xs) y = if y==1 then xs else x:minrow xs (y-1)

--Определитель
dety :: [[Float]] ->Float
dety [[x]] = x
dety (x:xs) = sum [(-1)^(1+j) * x!!(j-1) * dety (minstolb xs j) | j<- [1..length x]]

-- Вспомогательная функция к основной
-- (Считает дополнения ко всей строке и делит на детерминант)
rematrx1 ::[[Float]] -> Int -> [Float] 
rematrx1 x y=  [((-1)^(y+j) * dety (minrow (minstolb x j) y))/ dety x | j<-[1..length x]]

--Основная функция обратной матрицы
rematrx :: [[Float]] -> [[Float]] 
rematrx x = transp ([c | c <- [rematrx1 x y | y<-[1..length x]]])