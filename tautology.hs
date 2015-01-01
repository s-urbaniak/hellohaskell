int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

bools :: Int -> [[Bool]]
bools n = map (map conv . make n . int2bin) [0..limit]
          where
            limit = (2 ^ n) - 1
            make n bs = take n (bs ++ repeat 0)
            conv 0 = False
            conv 1 = True

