list = [1,2,3,4]

suma xs = case xs of
	[] -> 0
	head:tail -> head + suma tail

factorial x = if x == 0 then 1 else x * factorial (x - 1)

loop res index n = if index > n then res else loop (res * index ) (index + 1) n
factorialIter x = loop 1 1 x


powerIter x y = powerLoop 1 x y
powerLoop res x y = if y == 0 then res else powerLoop (x * res) x (y - 1)
