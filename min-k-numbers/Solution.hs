-- min-k-numbers
module Solution where
    import Data.List (sort)
    solution :: [Integer] -> Int -> [Integer]
    solution xs k = take k $ sort xs
