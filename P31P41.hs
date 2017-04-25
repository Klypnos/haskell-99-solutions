module P31P41 where
import Prelude hiding (gcd)
import Data.List
--p31
isPrime :: (Integral a) => a ->  Bool
isPrime n
    | n < 2 = False
    | otherwise = and $ map (\x -> n `mod` x /= 0) [2 .. floor (sqrt ( fromIntegral  (n))) ]

--p32
gcd :: Int -> Int -> Int
gcd x 0 = x
gcd x y = gcd y $ x `mod` y

--p33
coprime :: Int -> Int -> Bool
coprime a b = (==) 1 $ gcd a b

--p34
totient :: Int -> Int
totient a = (+) 1 $ length $ filter (==True) $ map (\x -> coprime a x ) [2 .. a]

--p35
primeFactor :: Int -> [Int]
primeFactor n = primeFactor' 2 n
    where
        primeFactor' a n
            | n == 1 = []
            | n `mod` a == 0 = a:(primeFactor' a $ n `div` a)
            | otherwise = primeFactor' (a+1) n --could be improved but im too lazy >_<

--p36
prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult a = zip (nub res) resM
    where
        res = primeFactor a --[3,3,5,7]
        resG = group $ res -- [[3,3],[5],[7]]
        resM = map (\x -> length x) resG -- [2,1,1]

--p37
phi :: Int ->  Int
phi a = foldl (*) 1 $ map (\(a,b) -> (a-1)*a^(b-1) ) $prime_factors_mult a

--p38
--nothing to do

--p39
primesR :: Int -> Int -> [Int]
primesR a b = filter (\x -> isPrime x ) [a..b]

--p40
goldbach :: Int -> (Int , Int)
goldbach a = head $ map (\b -> (a-b,b) ) $filter (\b -> isPrime b && isPrime (a-b)) [2 .. a `div` 2]

--p41
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList a b = map goldbach $ filter (\x -> x `mod` 2 == 0) [a .. b]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' a b c = filter (\(x,y) -> x > c && y > c ) $ map goldbach $ filter (\x -> x `mod` 2 == 0) [a .. b]
