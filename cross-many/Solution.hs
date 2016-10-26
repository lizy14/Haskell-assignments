-- cross-many
module Solution where
    swap i (xs, ys) = (
        take i xs ++ [(!!) ys i] ++ drop (i + 1) xs,
        take i ys ++ [(!!) xs i] ++ drop (i + 1) ys)
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
                    (\x ->
                        and [
                            x < length xs,
                            x < length ys
                        ]
                    )
                    is
                )
            )
            (
                take
                    (minimum $ map length [xs, ys])
                    xs,
                take
                    (minimum $ map length [xs, ys])
                    ys
            )

{- tests
:{
[
solution [3,17] "haskell" "python"   == ("hashel", "pytkon"),
solution [-7,0,2] "haskell" "python" == ("patkel", "hyshon"),
solution [3] "haskell" []            == ([], []),
solution [0..] "haskell" "python"    == ("python", "haskel")
]
:}
-}
