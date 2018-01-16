module List where

import Data.List

-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast = foldl1 $ curry snd -- convert snd to curried form
-- also foldr1 (const id)
-- consider the list [1, 2, 3, 4]. foldr1 over it is f 1 (f 2 (f 3 4)). so, we want 'f' to be a function, that given x,
-- returns another function that given y returns y. the latter is nothing but 'id'.
-- 'f id' needs to return 'id', which by definition, is 'const'.
-- ghci> :t const id
-- const id :: b -> a -> a

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x:y:xs) = case compare (length xs) 1 of
    LT -> x
    GT -> myButLast xs
    EQ -> y

myButLast' :: [a] -> a
myButLast' xs = snd . head . dropWhile penultimate $ zip [1..] xs
    where penultimate (i, _) = i /= (length xs) - 1

myButLast'' :: [a] -> a
myButLast'' = fst . foldl (\(_, x) y -> (x, y)) (empty, singleton)
    where empty = error "empty list"
          singleton = error "singleton list"

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k
    | k > length xs = error "index too large"
elementAt (x:xs) k = fst $ foldl (\(a, i) b -> if i >= k then (a, i + 1) else (b, i + 1)) (x, 1) xs

-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength = foldl (const . (+1)) 0
-- foldr (\_ -> (+1)) 0

-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (\acc y -> y : acc) []

myReverse' :: [a] -> [a]
myReverse' = foldr (\x acc -> acc ++ [x]) []

-- 6. Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [x] = True
isPalindrome (x:xs) = head last == x && isPalindrome list
    where (list, last) = splitAt n xs
          n = length xs - 1

-- 7. Flatten a nested list structure.
flatten :: [Either a [a]] -> [a]
flatten = foldl (\x y -> x ++ flatMap y) []
    where flatMap a = case a of (Left b) -> [b]
                                (Right b) -> b >>= \x -> [x]

-- 8. Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile ( == x) xs)

-- 9. Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = foldr f []
    where f x xs = case xs of
                             (b@(a:_):c) | x == a -> (x : b) : c
                             otherwise -> [x] : xs
--span :: (a -> Bool) -> [a] -> ([a], [a])
--span, applied to a predicate p and a list xs, returns a tuple where first element is longest prefix (possibly empty)
--of xs of elements that satisfy p and second element is the remainder of the list
--pack (x:xs) = let (first,rest) = span (==x) xs
--               in (x:first) : pack rest
--pack [] = []

-- 10. Run-length encoding of a list. Use the result of problem P09.
encode :: (Eq a) => [a] -> [(Int, a)]
-- can replace pack with 'group'
encode = map (\x -> (length x, head x)) . pack
-- map (length &&& head) $ group
-- [(length x, head x) | x <- group xs]
