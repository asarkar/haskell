module Recursion (bubblesort, mergesort, quicksort) where

bubblesort' :: (Ord a) => [a] -> ([a], Bool)
bubblesort' [] = ([], False)
bubblesort' [x] = ([x], False)
bubblesort' (x:y:xs)
    | x < y = (x : tc, swapped || False)
    | otherwise = (y : tc, swapped || True)
    where (tc, swapped) = bubblesort' (bigger : xs)
          bigger = max x y

bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs @ _
    | swapped = bubblesort tc
    | otherwise = tc
    where (tc, swapped) = bubblesort' xs

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
-- the idea is that we merge 2 sorted lists, thus the sublists are recursively sorted
mergesort xs = mergelist' (mergesort l) (mergesort r)
    where (l, r) = splitlist' xs

splitlist' :: [a] -> ([a], [a])
splitlist' xs @ _ = (l, r)
    where l = take n xs
          r = drop n xs
          n = length xs `div` 2

mergelist' :: (Ord a) => [a] -> [a] -> [a]
mergelist' x [] = x
mergelist' [] x = x
mergelist' (x:xs) (y:ys) -- put the smaller one in the front and recursively merge the rest
    | x < y = x : mergelist' xs  (y : ys)
    | otherwise = y : mergelist' (x:xs) ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort l ++ quicksort r
    where (l, r) = quicksort' (x:xs) x [] []
-- single-element and multi-element list steps can be replaced with the following list comprehensions
-- quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

quicksort' :: (Ord a) => [a] -> a -> [a] -> [a] -> ([a], [a])
quicksort' [] _ _ _ = ([], [])
quicksort' [x] pivot l r
    | x <= pivot = (x:l, r)
    | otherwise = (l, x:r)
quicksort' (x:xs) pivot l r
    | x <= pivot = quicksort' xs pivot (x:l) r
    | otherwise = quicksort' xs pivot l (x:r)
