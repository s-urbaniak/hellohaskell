putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStr' "\n"

getLine' = get []
get :: String -> IO String
get xs = do
         x <- getChar
         case x of
             '\n'-> return xs
             _ -> get (xs ++ [x])

interact' f = do input <- getLine'
                 putStrLn' (f input)

seq1 :: Monad m => [m a] -> m [a]
seq1 [] = return []
seq1 (m : ms) = m >>= \a -> do as <- seq1 ms
                               return (a : as)

seq5 :: Monad m => [m a] -> m [a]
seq5 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x : xs)

seq8 :: Monad m => [m a] -> m [a]
seq8 [] = return []
seq8 (m : ms) = do a <- m
                   as <- seq8 ms
                   return (a : as)

p a = getLine' >>= \b -> return (a == b) 

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs) = do flag <- p x
                         ys <- filterM' p xs
                         if flag then return (x : ys) else return ys

seq_ [] = return ()
seq_ (m : ms) = m >> seq_ ms

mapm1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapm1 f as = seq1 (map f as)

mapm2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapm2 f [] = return []
mapm2 f (a : as) = f a >>= \ b -> mapm2 f as >>= \ bs -> return (b : bs)

mapm6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapm6 f [] = return []
mapm6 f (a : as) = do b <- f a
                      bs <- mapm6 f as
                      return (b : bs)

mapm7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapm7 f [] = return []
mapm7 f (a : as) = f a >>= \ b -> do bs <- mapm7 f as
                                     return (bs ++ [b])

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x : xs) = do xn <- f a x
                            foldLeftM f xn xs

-- foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a [] = return a
foldRightM f a (x : xs) = do xn <- foldRightM f a xs
                             f x xn

-- foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r
