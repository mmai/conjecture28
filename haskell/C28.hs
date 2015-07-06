import Data.Numbers.Primes
import Data.List
import System.Environment

n = 1000000
sumIntN = take n sumInt
sumPrimesN = take n sumPrimes
sumNonPrimesN = take n sumNonPrimes

sumInt = scanl1 (+) [1..]
sumPrimes = scanl1 (+) primes
sumNonPrimes = scanl1 (+) nonprimes

nonprimes = filter (not . isPrime) [1..]
 
main :: IO ()
main = do 
  print (intersect sumIntN (intersect sumPrimesN sumNonPrimesN))
  print ( minimum [last sumIntN, last sumPrimesN, last sumNonPrimesN])

 
