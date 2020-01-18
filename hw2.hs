import Control.Monad
import Data.IORef 
import Control.Applicative
import Data.List
primes :: [Int]
primes = sieve[2..]
    where
        sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p> 0] --リスト内法表記
nthPrime :: Int ->Int
nthPrime  n = primes !! n

main :: IO()
main = do
    putStrLn "test"