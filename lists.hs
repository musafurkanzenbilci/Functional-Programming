-- naturals: The infinite list of natural numbers
naturals :: [Integer]
naturals = [0..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x:xs) (y:ys) = x:(y:(interleave xs ys))

-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
integers = interleave naturals [(-1)*x | x<-naturals,x>0]