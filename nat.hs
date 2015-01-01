import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)

add n (Succ m) = Succ (add m n)
add n Zero = Zero

