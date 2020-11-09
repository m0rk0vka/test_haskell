{-
    Zdec' znakomimsya s haskell cherez ohaskell.guide
-}

{-# LANGUAGE MultiWayIf #-}

import Data.Char

-- MultiWayIf construction
checkLocalHost :: String -> String
checkLocalHost ip = 
    if ip == "0.0.0.0" || ip == "127.0.0.1"
        then "It's localhost!"
        else "It's not localhost!"

-- _ - universalnyi obrazec
analyzeGold :: Int -> String
analyzeGold 999 = "Wow! 999 standard!"
analyzeGold 750 = "Great! 750 standard."
analyzeGold 585 = "Not bad! 585 standard."
analyzeGold _ = "I don't know such a standard..."

-- let in && where expr
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40 in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    correction = 120
    delta      = 8

-- spiski
addTo :: String -> [String] -> [String]
addTo newHost hosts = newHost : hosts

-- tuple
chessMove :: String
          -> (String, String)
          -> (String, (String, String))
chessMove color (from, to) = (color, (from, to))

-- operator
f <+> g = f . g

-- mapping
ten :: [Double] -> [Double]
ten = map (\n -> n * 10)


pretty :: [String] -> [String]
pretty = map (stars . big)
  where
    big = map toUpper
    stars = \s -> "* " ++ s ++ " *"


main :: IO ()
--main = putStrLn (head ["Vim", "Emacs", "Atom"])
--main = print (tail ["Vim", "Emacs", "Atom"])
{-main = print ("127.0.0.1" `addTo` hosts)
    where hosts = ["1.1.1.1", "2.2.2.2"]-}
{-main = print (color ++ ": " ++ from ++ "-" ++ to)
  where (color, (from, to)) = chessMove "white" ("e2", "e4")-}
--main = putStrLn <+> checkLocalHost $ "127.0.0.1"
--main = print . ten $ [1.2, 1,4, 1.6]
--main = print . pretty $ ["haskell", "lisp", "coq"]
main =
    let
        cx = 2 / 10
        nk = 10 / 2
        coeff = [cx, nk]
    in
        print $ length coeff
