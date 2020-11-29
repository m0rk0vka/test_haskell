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


inverseMat :: [[Double]] -> [[Double]]
inverseMat x = mul x 0
    where
        mul x i
            | i < length(x)-1 = (map (\c -> c * (1 / (det x))) ((attach x)!!i)):(mul x (i+1))
            | otherwise = [map (\c -> c * (1 / (det x))) ((attach x)!!i)]

        det [[x]] = x
        det [[a,b],[c,d]] = a * d - c * b
        det ([]:xss) = 0
        det ((x:xs):xss) = x * (-1)^(length(xss) - length(xs) 
                + 2) * detMat[delN (length(xss) - length(xs)) y | y <- xss] + detMat((xs):xss)

        attach x = attach1 x 0
        attach1 x i 
            | i < length(x) - 1 = (attach2 x i 0):(attach1 x (i+1))
            | otherwise = [attach2 x i 0]
        attach2 x i j
            | j < length(x!!i) - 1 = (((-1)^(length(delNN i x) - length([x!!i!!k | k <- [0..(length(x!!i))], k /= j]) + 2) * det[(delN (length(delNN i x) - length([x!!i!!k | k <- [0..(length(x!!i))], k /= j])) y) | y <- (delNN i x)]):(attach2 x i (j+1)))
            | otherwise = [(-1)^(length(delNN i x) - length([x!!i!!k | k <- [0..(length(x!!i))], k /= j]) + 2) * det[(delN (length(delNN i x) - length([x!!i!!k | k <- [0..(length(x!!i))], k /= j])) y) | y <- (delNN i x)]]
            
        
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