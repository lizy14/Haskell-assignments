-- water-in-container

-- reference: https://github.com/liuxinyu95/AlgoXY/blob/pub/search/bfs/WaterJugs.hs

solution :: Integer -> Integer -> Integer -> Integer
solution a b g = fromIntegral (length (bruteForce a b g [[(0, 0)]]) - 1)

bruteForce a b g (c:cs)
    -- brute force solution
    -- keep track of previous states to avoid infinite loop
    | or [fst (head c) == g, snd (head c) == g]
        = reverse c
    | otherwise
        = bruteForce a b g (cs ++ map (:c) (nextStatesFiltered a b c))

nextStates a b ((x, y):ps) =
    map
        (\f -> f x y)
        [
            \_ y -> (a, y), -- x to v
            \x _ -> (x, b), -- x to v
            \_ y -> (0, y), -- a to v
            \x _ -> (x, 0), -- b to v
            \x y -> (max 0 (x + y - b), min (x + y) b), -- a to b
            \x y -> (min (x + y) a, max 0 (x + y - a))  -- b to a
        ]

nextStatesFiltered a b p =
    filter
        (\z -> notElem z (tail p))
        (nextStates a b p)
