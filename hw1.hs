-- napisat' func kotoraya stavit posledni element na pervoe mesto bez vstroennyh func
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

-- zadacha napisat' swapFirstAndLast odnoi funciei
swfl :: [a] -> [a]
swfl [] = error "Empty list"
swfl [x] = [x]
swfl (x:xs) =
    let
        (f, g) = sw x xs
        sw x [y] = (y, [x])
        sw x (y:ys) =
            let
                (f, g) = sw x ys
            in
                (f, y:g)
    in
        f:g