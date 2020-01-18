import Control.Monad
import Data.IORef 
import Control.Applicative
import Data.List
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)



pre_sine ::Double -> (Integer -> Integer) -> Integer->Double
pre_sine x f 0 = x
pre_sine x f k= pre_sine x f (k-1) + (-1)^k*x^(2*k+1)/fromIntegral (f (2*k+1))

sine ::Double ->Double
sine x=pre_sine x fact 100

pre_cosine ::Double ->(Integer -> Integer) -> Integer->Double
pre_cosine x f 0 = 1
pre_cosine x f k= pre_cosine x f (k-1) + (-1)^k*x^(2*k)/fromIntegral (f (2*k))

cosine ::Double -> Double
cosine x=pre_cosine x fact 100

main :: IO ()
main = do
    putStr "sinx=" 
    print $ pre_sine (pi/fromIntegral(6)) fact 100
    putStr "cosx=" 
    print $ pre_cosine (pi/fromIntegral(6)) fact 100