-- bombe-1
module Solution where
    import Data.Char (ord)
    type Bit = Int

    int2bits 0 = []
    int2bits n = n `mod` 2 : int2bits (n `div` 2)

    make8 :: [Bit] -> [Bit]
    make8 bits = take 8 (bits ++ repeat 0)

    getParityBit :: [Bit] -> Bit
    getParityBit bits = (length $ filter (\x -> x == 1) bits) `mod` 2

    wrapByte :: [Bit] -> [Bit]
    wrapByte bits = [getParityBit $ make8 bits] ++ (make8 bits)

    solution :: String -> [Bit]
    solution str = concat $ map ((\x -> wrapByte $ int2bits $ ord x)) str
