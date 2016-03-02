-- sieve of eratosthenes!!!!

primes :: [Int]
primes = sieve [2..] -- an infinite list! (or rather, a potentially-infinite list)

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]