module P26P40 where
import Data.List

--p26 :((((((
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ xs !! i : y |  i <- [0 .. length xs - 1],
                                     y <- combinations (n-1) (drop (i+1) xs)  ]

--p27 :(((((((((((((((((((((
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
    where
        ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
        ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
groupp :: [Int] -> [a] -> [[[a]]]
groupp [] _ = [[]]
groupp (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- groupp ns rs ]

--p28
lsort :: Ord a => [[a]] -> [[a]]
lsort []  = []
lsort (x:xs) = ins (lsort xs) x

ins :: Ord a => [[a]] -> [a]-> [[a]]
ins [] y  = [y]
ins xs@(x':xs') y
    | length y <= length x' = y:xs
    | otherwise = x': ins xs' y

lfsort :: Ord a => [[a]] -> [[a]] -- :(
lfsort lists = concat groups
    where groups = lsort $ groupBy (\x y -> length x == length y) $ lsort lists
