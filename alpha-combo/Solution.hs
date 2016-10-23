-- alpha-combo
module Solution where
    import Data.List (subsequences)
    solution :: Integer -> [[Char]]
    combinations k ns = filter ((k==).length) (subsequences ns)
    solution i = combinations (fromIntegral i) "abcdefghijklmnopqrstuvwxyz"
