module P1P13 where
import Data.List hiding((!!))
import Prelude hiding ((!!))

-- p1
myLast :: [a] -> a
myLast xs
    | null $ tail xs = head xs
    | otherwise     =  myLast $ tail xs

--p2
mybutLast :: [a] -> [a]
mybutLast xs
    | null $ tail xs = []
    | otherwise = head xs: mybutLast ( tail xs )

--p3
(!!) :: [a] -> Integer -> a
(!!) xs ind
    | ind == 0 = head xs
    | otherwise = tail xs !! (ind - 1)

--p4
myLength :: [a] -> Integer
myLength [] = 0
myLength xs = (+) 1 $ myLength $ tail xs

--p5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--p6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
    | length xs < 2 = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome $ init xs
    | otherwise = False

--p7 :(
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten xs = case xs of
    Elem xs -> [xs]
    List (x:xs') -> flatten x ++ flatten (List xs')
    List [] -> []

--p8
compress :: Ord a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress xs@(x':xs')
    | x' == head xs' = compress xs'
    | otherwise = x': compress xs'

--p9 :(
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : pack ( dropWhile (==x) xs )

--p10
encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode xs = (length (head b), head  (head b) ): (encode $ dropWhile (==head xs) xs)
    where
        b = pack xs

--p11 ASD
data Singularity a = Single a | Multiple Int a
    deriving Show
encodeModified :: Eq a => [a] -> [Singularity a]
encodeModified [] = []
encodeModified ys
    | fst x' == 1 = Single (snd x') : (encodeModified $ drop 1 ys)
    | otherwise = Multiple (fst x') (snd x') : (encodeModified $ drop (fst x') ys)
        where
            xs@(x':xs') = encode ys

--p12

decodeModified :: Eq a => [Singularity a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
    Single a -> a:(decodeModified xs)
    Multiple n a -> (replicate n a) ++ (decodeModified xs)

--p13
encodeDirect :: Eq a => [a] -> [Singularity a]
encodeDirect [] = []
encodeDirect (x:xs) = if length ys > 1
    then Multiple (length ys) (head ys) : (encodeDirect (drop (length ys - 1) xs))
    else Single (head ys) : (encodeDirect xs)
        where
             ys = x: (takeWhile (==x) xs)
