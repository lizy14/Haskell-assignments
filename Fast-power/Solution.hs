pow :: Integer -> Integer -> Integer
pow m 0 = 1
pow m n =
  if even n
    then
      (pow m (div n 2))^2
    else
      m * pow m (n-1)

solution :: Integer -> Integer -> Integer
solution = pow
