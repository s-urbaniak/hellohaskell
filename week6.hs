import Data.Binary

takeW :: (a -> Bool) -> [a] -> [a]
takeW _ [] = []
takeW p (x : xs)
	| p x = x : takeW p xs
	| otherwise = []

takeW2 :: (a -> Bool) -> [a] -> [a]
takeW2 p = foldl (\ acc x -> if p x then x : acc else acc) []

dropW :: (a -> Bool) -> [a] -> [a]
dropW _ [] = []
dropW p (x : xs)
	| p x = dropW p xs
	| otherwise = x : xs


dropW2 :: (a -> Bool) -> [a] -> [a]
dropW2 p = foldr (\ x acc -> if p x then acc else x : acc) []

map2 f = foldl (\ xs x -> xs ++ [f x]) []

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin_ = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8_ = unfold null (take 8) (drop 8)
map_ f = unfold null (f . head) tail
iterate_ f = unfold (const False) id f
