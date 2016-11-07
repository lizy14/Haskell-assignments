-- enigma-1
module Solution where
    import Data.Char (chr)
    type Bit = Int

    bits2int :: [Bit] -> Int
    bits2int bits =
        sum [ w * b | (w, b) <- zip weights bits]
            where weights = (iterate (\x -> 2 * x) 1)

    getParityBit :: [Bit] -> Bit
    getParityBit bits = (length $ filter (\x -> x == 1) bits) `mod` 2

    chop9 :: [Bit] -> [[Bit]]
    chop9 [] = []
    chop9 bits = take 9 bits : chop9 (drop 9 bits)

    verifyWrappedByte :: [Bit] -> Bool
    verifyWrappedByte bits =
        (head bits) == (getParityBit $ tail bits)

    solution :: [Bit] -> String
    solution bits =
        map
            (\x ->
                if verifyWrappedByte x
                    then chr $ bits2int $ tail x
                    else error "Parity bit verification failed."
            )
            (chop9 bits)
