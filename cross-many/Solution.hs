-- cross-many
module Solution where
    swap i (xs, ys) = (
        take i xs ++ [(!!) ys i] ++ drop (i + 1) xs,
        take i ys ++ [(!!) xs i] ++ drop (i + 1) ys)

    longer :: [a] -> [a] -> Bool
    longer xs ys =
        if null xs
            then
                False
            else
                if null ys
                    then
                        True
                    else
                        longer
                            (tail xs)
                            (tail ys)

    minLength :: [a] -> [a] -> Int
    minLength xs ys = length $ if longer xs ys then ys else xs

    helper is (xs, ys) =
        if null is
            then
                (xs, ys)
            else
                helper
                    (tail is)
                    (swap (head is) (xs, ys))

    solution :: [Int] -> [a] -> [a] -> ([a], [a])
    solution is xs ys =
        helper
            (filter
                (\x -> x >= 0)
                (takeWhile
                    (\x -> x < minLength xs ys)
                    is))
            (
                take (minLength xs ys) xs,
                take (minLength xs ys) ys
            )

{- tests
:{
[
solution [3,17] "haskell" "python"   == ("hashel", "pytkon"),
solution [-7,0,2] "haskell" "python" == ("patkel", "hyshon"),
solution [3] "haskell" []            == ([], []),
solution [0..] "haskell" "python"    == ("python", "haskel"),
solution [0..] [1,3..] [2,4..10]     == ([2,4,6,8,10],[1,3,5,7,9]),
solution [0,2..] [1,3..] [2,4..10]   == ([2,3,6,7,10],[1,4,5,8,9])
]
:}
-}
