--napisat' func kotoraya stavit posledni element na pervoe mesto bez vstroennyh func
last1 :: [a] -> a
last1 [] = error "EmptyList"
last1 [x] = x
last1 (x:xs) = last1 xs


init1 :: [a] -> [a]
init1 [] = error "EmptyList"
init1 [x] = []
init1 (x:xs) = x : init1 xs


swapFirstAndLast :: [a] -> [a]
swapFirstAndLast [] = error "EmptyList"
swapFirstAndLast [x] = [x]
swapFirstAndLast (x:xs) = (last1 xs : init1 xs) ++ [x]
--ne znay kak napisat' bez vspomogatel'nyh swapFirstAndLast