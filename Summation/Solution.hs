sumAlongNext :: (Integer->Integer)->Integer->(Integer->Integer)->Integer->Integer
sumAlongNext f m next n =
  if m > n
    then 0
    else if (next m) > n
      then f m
      else f m + sumAlongNext f (next m) next n

sumOfF :: (Integer->Integer)->Integer->Integer->Integer
sumOfF f m n = sumAlongNext f m (\x -> x+1) n

sumOfCubes :: Integer->Integer->Integer
sumOfCubes m n = sumOfF (\x -> x^3) m n
