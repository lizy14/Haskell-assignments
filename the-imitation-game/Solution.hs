-- the-imitation-game
module Solution where
    import Data.Char (ord, chr)
    type Bit = Int

    bits2int :: [Bit] -> Int
    bits2int bits =
        sum [ w * b | (w, b) <- zip weights bits]
            where weights = (iterate (\x -> 2 * x) 1)

    int2bits :: Int -> [Bit]
    int2bits 0 = []
    int2bits n = n `mod` 2 : int2bits (n `div` 2)

    make8 :: [Bit] -> [Bit]
    make8 bits = take 8 (bits ++ repeat 0)

    encode :: String -> [Bit]
    encode str = concat $ map (\x -> make8 $ int2bits $ ord x) str

    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 bits = take 8 bits : chop8 (drop 8 bits)

    decode :: [Bit] -> String
    decode bits = map (\x -> chr $ bits2int x) (chop8 bits)
