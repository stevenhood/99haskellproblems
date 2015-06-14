{- ---------- Ninety-Nine Problems in Haskell ---------- -}
-- Steven Hood

{- ----- Lists ----- -}

-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast = head . reverse

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- 3. Find the K'th element of a list. The first element in the list is
--    number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (x:xs) n
    | n < 1 = error "Index out of bounds"
    | otherwise = elementAt xs (n - 1)

-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength xs = accum xs 0
    where accum [] n = n
          accum (x:xs) n = accum xs (n + 1)

-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward
--    or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7. Flatten a nested list structure.
--    Transform a list, possibly holding lists as elements into a `flat' list
--    by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- 8. Eliminate consecutive duplicates of list elements.
--    If a list contains repeated elements they should be replaced with a
--    single copy of the element. The order of the elements should not be
--    changed.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

-- 9. Pack consecutive duplicates of list elements into sublists. If a list
--    contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = dups : pack rest
    where dups = x : takeWhile (== x) xs
          rest = dropWhile (== x) xs

-- 10. Run-length encoding of a list. Use the result of problem P09 to
--     implement the so-called run-length encoding data compression method.
--     Consecutive duplicates of elements are encoded as lists (N E) where N is
--     the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack
