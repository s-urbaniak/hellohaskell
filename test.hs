import Debug.Trace

double x = x + x

quadruple x =
    double (double x)

factorial n =
    product [1..n]

average ns =
    sum ns `div` length ns


n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

last1 xs = drop (length xs - 1) xs
last2 xs = head (drop (length xs - 1) xs)
last3 xs = tail (reverse xs)
last4 xs = reverse (head xs)
last5 xs = xs !! (length xs - 1)
last6 xs = head (drop (length xs) xs)
last7 xs = head (reverse xs)
last8 xs = reverse xs !! (length xs - 1)

init1 xs = tail (reverse xs)
init2 xs = reverse (head (reverse xs))
init3 xs = reverse (tail xs)
init4 xs = take (length xs) xs
init5 xs = reverse (tail (reverse xs)) --
init6 xs = take (length xs - 1) (tail xs)
init7 xs = drop (length xs - 1) xs
init8 xs = take (length xs - 1) xs -- Wrong because it returns an empty list instead of an error

sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

product1 [] = 1
product1 (x : xs) = x * product1 xs

q1 [] = []
q1 (x : xs) = q1 larger ++ [x] ++ q1 smaller
              where smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

q2 [] = []
q2 (x : xs) = reverse(q2 smaller ++ [x] ++ q2 larger)
              where smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

q3 [] = []
q3 (x : xs) = q3 larger ++ q3 smaller ++ [x]
              where smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

q4 [] = []
q4 (x : xs) = reverse (q4 smaller) ++ [x] ++ reverse (q4 larger)
              where smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

q5 [] = []
q5 (x : xs) = q5 larger ++ [x] ++ q5 smaller
             where smaller = [a | a <- xs, a < x || a == x]
                   larger = [b | b <- xs, b > x]

q6 [] = []
q6 (x : xs) = q6 larger ++ [x] ++ q6 smaller
              where smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b > x]

q7 [] = []
q7 (x : xs) = 
    reverse
        (reverse (q7 smaller) ++ [x] ++ reverse(q7 larger))
              where smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

q8 [] = []
q8 (x : xs) = q8 larger ++ q8 smaller
              where x = maximum xs
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b >= x]

addmap1 = map (+1)
addmap2 = map (\x -> x + 1)

myfold f a [] = a
myfold f a (x : xs) = f x (myfold f a xs)

len = myfold count 0
      where count a n = n + 1

rsum = foldr (+) 0
lsum = foldl (+) 0

