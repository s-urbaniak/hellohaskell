import Data.Char

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = int2let ((let2int c + n + 32) `mod` 26 - 32)
    | otherwise = c

encode n xs = [shift n x | x <- xs]

