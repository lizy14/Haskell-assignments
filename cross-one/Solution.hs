-- cross-one
module Solution where
    solution :: Int -> [a] -> [a] -> ([a], [a])
    solution i xs ys = (
        (take i ys) ++ (drop i xs),
        (take i xs) ++ (drop i ys))

{- tests
:{
[
solution 3 "haskell" "python"    == ("pytkell","hashon"),
solution 2 "haskell" []          == ("skell","ha"),
solution 12 "haskell" "meep"     == ("meep","haskell"),
solution (-1) "haskell" "python" == ("haskell","python")
]
:}
-}
