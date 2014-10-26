-- import Prelude hiding ((&&))
-- import Prelude hiding ((||))

-- halve1 doesn't compile

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

halve4 xs = splitAt (length xs `div` 2)

halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

halve6 xs = splitAt (div (length xs) 2) xs

-- halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

stail1 xs = if null xs then [] else tail xs

stail2 [] = []
stail2 (_ : xs) = xs

stail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

stail4 xs
  | null xs = []
  | otherwise = tail xs

-- stail5 doesn't compile

stail6 [] = []
stail6 xs = tail xs

stail7 [x] = [x]
stail7 (_ : xs) = xs

stail8 = \ xs ->
  case xs of
    [] -> []
    (_ : xs) -> xs

t = [(x, y) | x <- [True, False], y <- [True,False]]

checkor = map (\(x,y) -> x||y) t == [True,True,True,False]
checkand = map (\(x,y) -> x&&y) t == [True,False,False,False]
showand = map (\(x,y) -> ((x,y),x&&y)) t

-- import Prelude hiding ((||))

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

-- b || c
--   | b == c = True
--   | otherwise = False

-- b || c
--   | b == c = b
--   | otherwise = True

-- b || False = b
-- _ || True = True

-- b || c
--   | b == c = c
--   | otherwise = True

-- b || True = b
-- _ || True = True

-- False || False = False
-- False || True = True
-- True || False= True
-- True || True = True

-- True && True = True
-- _ && _ = False

-- a && b = if a then if b then True else False else False
-- a && b = if not (a) then not (b) else True
-- a && b = if a then b
-- a && b = if a then if b then False else True else False
-- a && b = if a then b else False
-- a && b = if b then a else False

