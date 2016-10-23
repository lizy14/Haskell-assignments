-- sum-numbers
module Solution where
    import Data.Char (isDigit, digitToInt, intToDigit)
    import Data.List.Split (splitOn)

    stringToInt :: Int -> String -> Int
    stringToInt base str =
        if null str
            then
                0
            else
                base * (stringToInt base $ init str)
                + (digitToInt $ last str)

    isInt :: String -> Bool
    isInt x = and $ map isDigit x

    solution :: String -> Int
    solution x =
        sum $ map (stringToInt 10) (filter isInt $ splitOn " " x)
