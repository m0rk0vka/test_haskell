--funkcia udvaivania
test1 :: Int -> Int --signatura (vhod->vyhod)
test1 x = x + x


--funkcia c vyzovom predydyshei funkcii
test2 :: Int -> Int -> Int -- dva Int na vhod 
test2 x y = test1 x + test1 y


--funkcia sravnenia c obrazcom
test3 :: Int -> String
test3 1 = "one"
test3 5 = "five"
test3 x = "no one and no five"


--funckcia nahojdenia maximuma dvyx chisel
max1 :: (Ord) a => a -> a -> a -- (Ord) - ot slova order - poryadok (class sravneni)
max1 x y                       -- construkcia nazyvaetcya gvar (gvardia)
    | x <= y = y
    | otherwise = x          -- otherwise


--funkcia max dlya treh chisel
max2 :: (Ord) a => a -> a -> a -> a
max2 x y z
    | x >= y && x >= z = x
    | z >= y && z >= x = z
    | otherwise = y

{--
rabota so spiskami
Prelude> 1:2:3:[]++[4]
[1,2,3,4]
Prelude> head [1,2,3,4]
1
--}
head1 :: [a] -> a
head1 [] = error "skip"
head1 [x] = x
head1 (x:xs) = head1 [x] --constructor spiskov (x:xs) x - pevyi element, xs - vse ostalnoe
{--
Prelude> last [1,2,3,4]
4
--}
last1 :: [a] -> a
last1 [] = error "skip"
last1 [x] = x
last1 (x:xs) = last1 xs
--funkcia vydaet predposlednyi element
prelast :: [a] -> a
prelast [] = error "two more"
prelast [x] = error "one more"
prelast [x, y] = x
prelast (x:xs) = prelast xs
{--
Prelude> init [1,2,3,4]
[1,2,3]
Prelude> tail [1,2,3,4]
[2,3,4]
Prelude> elem 3 [1,2,3,4]
True
Prelude> elem 5 [1,2,3,4]
False
Prelude> drop 3 [1,2,3,4,5]
[4,5]
Prelude> [1,2,3,4]!!1
2
--}
