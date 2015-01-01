module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

dup a = (a, a)
ddup :: (a) -> ((((a), (a)), ((a), (a))), (((a), (a)), ((a), (a))))
ddup = dup . dup . dup

-- (a -> (b -> a)) -> (a -> (b -> b))
-- ((b -> a) -> a) -> ((b -> a) -> b)
h :: ((a -> b) -> a) -> ((a -> b) -> b)
-- ((a -> b -> a) -> a -> b -> b)
h g f = (f . g) $ f

fix = h fix

-- f = \f n -> if (n == 0) then 1 else n * f (n - 1)

k f = fix $ f

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle (n + 1) = 1 + n + triangle n

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x : xs) = case a == x of
                     True -> 1 + count a xs
                     False -> count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x == y = x
  | x < y = euclid (x, y-x)
  | otherwise = euclid (x-y, y)

-- ===================================
-- Ex. 3
-- ===================================
idx xs = [(x,y) | (x,y) <- zip xs [0..]]

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g xs = funkyMap2 f g $ idx xs

funkyMap2 f g [] = []
funkyMap2 f g ((x,y) : xs)
  | even y = f x : funkyMap2 f g xs
  | odd  y = g x : funkyMap2 f g xs

