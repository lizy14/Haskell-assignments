-- alpha-combo
module Solution where
    import Data.List (subsequences, sort)
    solution :: Integer -> [[Char]]
    combinations k ns = filter ((k==).length) (subsequences ns)
    solution i = sort $ combinations (fromIntegral i) "abcdefghijklmnopqrstuvwxyz"
