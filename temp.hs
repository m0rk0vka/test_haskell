{-
    Zdec' znakomimsya s haskell cherez ohaskell.guide
-}

checkLocalHost :: String -> String
checkLocalHost ip = 
    if ip == "0.0.0.0" || ip == "127.0.0.1"
        then "It's localhost!"
        else "It's not localhost!"

main :: IO ()
main = putStrLn (checkLocalHost "0.0.0.0")
