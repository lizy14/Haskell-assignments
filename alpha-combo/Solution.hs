-- alpha-combo
module Solution where
    import Data.List (subsequences, sort)
    combinations k ns = filter ((k==).length) (subsequences ns)
    solution :: Integer -> [[Char]]
    solution i = sort $ combinations (fromIntegral i) ['a'..'z']
