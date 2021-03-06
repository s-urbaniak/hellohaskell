import Data.List

check (i,j) (m,n) = (j == n) || (i + j == m + n) || (i - j == m - n)

safe p n = and [not (check (i,j) (m,n)) | (i,j) <- zip [1..length p] p]
           where m = length p + 1

queens 0 c = [[]]
queens m c = [p ++ [n] | p <- queens (m-1) c,
                              n <- [1..c],
                              safe p n]

sneeuq 0 = [[]]
sneeuq m = [p ++ [n] | n <- [1..8],
                       p <- ps,
                       safe p n]
           where ps = sneeuq (m-1)

listValues xs = map (map xs)

main = putStrLn (intercalate " " ["[" ++ intercalate " " (map show xs) ++ "]" | xs <- queens 8 8])
