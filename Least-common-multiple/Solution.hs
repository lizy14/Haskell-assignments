gcd_ :: Integer -> Integer -> Integer
gcd_ x y = if not (y == 0) then gcd_ y (mod x y) else x

lcm_ :: Integer -> Integer -> Integer
lcm_ x y = div (x * y) (gcd_ x y)

solution :: Integer -> Integer -> Integer
solution = lcm_
