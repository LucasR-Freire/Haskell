--Perfect Numbers
{-A perfect number is a positive integer equal to the sum
    of its positive divisors, excluding the number itself-}

perfectNumsTo :: [Int]
perfectNumsTo = filter (\x-> isPerfectNum x) [1..]
    where   isPerfectNum n = n == sum (divisors n)
            divisors x = filter (\y -> x `mod` y ==0) [1..(x-1)]
