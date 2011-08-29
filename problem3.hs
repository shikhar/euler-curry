-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

primes = 2 : filter (\x -> (length (primeFactors x)) == 1) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise = factor n ps
 
answer = last (primeFactors 600851475143)
