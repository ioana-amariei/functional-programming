-- explicit type declaration
addTwo' :: Integer -> Integer -> Integer
addTwo' x y = x + y

power :: Integer -> Integer
power x = x * x

-- Write a function which computes the distance between two 2D points.
distance p1 p2 = power (snd(p1) - fst(p1)) + power(snd(p2) - fst(p2))

-- Write a function which computes factorial of a given integer.
-- If the integer is less than zero then your function should return 1.
factorial :: Integer -> Integer
factorial x = if x == 0 then 1 else x * factorial(x - 1)

-- Write a function which computes the ith number in the Fibonacci sequence.
fibonacci :: Integer -> Integer
fibonacci x = if x == 0 then 0 else (if x == 1 then 1 else fibonacci (x - 1) + fibonacci (x - 2))

-- suma lista
list = [1,2,3,4]

suma xs = case xs of
            	[] -> 0
            	head:tail -> head + suma tail


-- ridicare putere iterativ
powerIter x y = powerLoop 1 x y
powerLoop result x y = if y == 0 then result else powerLoop (x * result) x (y - 1)
