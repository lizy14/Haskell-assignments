-- queens
module Solution where
    import Data.Char (ord)

    type Queens = [Int]
    type Coordinate = (Char, Int)

    helper :: Int -> Int -> [Queens]
    --reference: http://dwz.cn/4As5kG
    helper 0 b = [[]]
    helper n b = [ x : y | y <- helper (n-1) b, x <- [1..b], safe x y 1]
        where
            safe x [] n = True
            safe x (c:y) n =
                and [
                    x /= c,
                    x /= c + n,
                    x /= c - n,
                    safe x y (n+1)
                ]

    columnID :: Char -> Int
    columnID c =
        ord c - ord 'a' + 1

    queenPresent :: Queens -> Coordinate -> Bool
    queenPresent solution (c, r) =
        if length solution < columnID c
            then False
            else solution !! (columnID c - 1) == r

    queensPresent :: Queens -> [Coordinate] -> Bool
    queensPresent solution positions =
        and $
            map
                (\x -> queenPresent solution x)
                positions

    solution :: [Coordinate] -> Int
    solution positions =
        (\x -> if null x then 0 else maximum x) $
            map
                (\x -> length x - length positions) $
                filter
                    (\x -> queensPresent x positions) $
                    concat $ map (\i -> helper i 8) [1..8]

-- solution [('d',8),('g',7),('c',6),('f',5)] == 3
