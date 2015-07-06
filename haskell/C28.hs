import Data.Numbers.Primes
import Data.List
import System.Environment

-- intersectSorted3 returns a lazy intersection of three ordered integer lists, along with each power of 10 reached in the testing (with a negative sign to differentiate from the real values of the intersection).
-- ie : [-1, 28, -10, -100, -1000, -10000, ...]
intersectSorted3:: Integer -> [Integer] -> [Integer] -> [Integer] -> [Integer]
intersectSorted3 _ [] _ _ = []
intersectSorted3 _ _ [] _ = []
intersectSorted3 _ _ _ [] = []
intersectSorted3 exp (x:xs) (y:ys) (z:zs)
        | x > (10 ^ exp) && (x < y)  && (x < z) = (-10 ^ exp):(intersectSorted3 (exp + 1) (x:xs) (y:ys) (z:zs))
        | x < y  && x < z = intersectSorted3 exp xs (y:ys) ( z:zs ) 
        | x < y  && x == z = intersectSorted3 exp xs ( y:ys ) zs 
        | x < y  && x > z = intersectSorted3 exp ( x:xs ) (y:ys) zs 
        | x == y  && x < z = intersectSorted3 exp xs ys (z:zs) 
        | x == y  && x == z = x:(intersectSorted3 exp xs ys zs)
        | x == y  && x > z = intersectSorted3 exp (x:xs) (x:ys) zs 
        | x > y  && x <= z = intersectSorted3 exp ( x:xs ) ys ( z:zs ) 
        | x > y  && x > z = intersectSorted3 exp ( y:ys ) ( x:xs ) ( z:zs ) 

-- list of the sums of integers : 1, 1+2, 1+2+3, ...
sumInt = scanl1 (+) [1..]

-- list of the sums of primes : 2, 2+3, 2+3+5, ...
sumPrimes = scanl1 (+) primes

-- list of the sums of non primes : 1, 1+4, 1+4+6, ...
sumNonPrimes = scanl1 (+) nonprimes

nonprimes = filter (not . isPrime) [1..]
 
main :: IO ()
main = do 
  print (intersectSorted3 0 sumInt sumPrimes sumNonPrimes)
 
