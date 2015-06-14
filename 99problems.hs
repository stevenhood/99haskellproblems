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

-- 11. Modified run-length encoding.
--     Modify the result of problem 10 in such a way that if an element has no
--     duplicates it is simply copied into the result list. Only elements with
--     duplicates are transferred as (N E) lists.
data Encode a = Multiple Int a | Single a
    deriving Show

encodeModified :: Eq a => [a] -> [Encode a]
encodeModified = map f . encode
    where f (1, x) = Single x
          f (n, x) = Multiple n x

-- 12. Decode a run-length encoded list.
--     Given a run-length code list generated as specified in problem 11.
--     Construct its uncompressed version.
decodeModified :: Eq a => [Encode a] -> [a]
decodeModified = concatMap f
    where f (Single x) = [x]
          f (Multiple n x) = replicate n x

-- 13. Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11,
-- simplify the result list by replacing the singleton lists (1 X) by X.
-- encodeDirect :: Eq a => [a] -> [Encode a]
-- TODO

-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

-- 15. Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n 1
    where helper [] _ _ = []
          helper (x:xs) m k
              | m == k    = helper xs m 1
              | otherwise = x : helper xs m (k + 1)
