-- decimal-to-binary
import Data.Char (digitToInt, intToDigit)

stringToInt :: Int -> [Char] -> Int
stringToInt base str =
    if null str
        then
            0
        else
            base * (stringToInt base (init str))
            + (digitToInt (last str))

intToString :: Int -> Int -> [Char]
intToString base x =
    if x == 0
        then
            ""
        else
            intToString base (div x base)
            ++ [intToDigit (mod x base)]

solution :: [Char] -> [Char]
solution x = intToString 2 (stringToInt 10 x)
