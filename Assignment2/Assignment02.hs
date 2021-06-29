module Assignment02 where

-- Imports just a few things that we have seen from the standard Prelude
-- module. (If there is no explicit 'import Prelude' line, then the entire
-- Prelude module is imported.)
import Prelude((+), (-), (*), (<), (>), (++), not, Bool(..), Char, undefined)

import Recursion

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

times :: Numb -> Numb -> Numb
times E n = E
times (S m) n = add n (times m n)

equal :: Numb -> Numb -> Bool
equal E E = True
equal E n = False
equal n E = False
equal (S m) (S n) = equal m n

bigger :: Numb -> Numb -> Numb
bigger E n = add E n
bigger n E = add n E
bigger (S m) (S n) = add one (bigger m n)

count :: (a -> Bool) -> [a] -> Numb
count x [] = zero
count x (y:ys) = case (x y) of
                        True -> add one (count x ys)
                        False -> add zero (count x ys)

remove :: (a -> Bool) -> [a] -> [a]
remove x [] = []
remove x (y:ys) = case (x y) of 
                        True -> remove x ys
                        False -> y : remove x ys

prefix :: Numb -> [a] -> [a]
prefix E x = []
prefix n [] = []
prefix (S m) (x:xs) = x : prefix m xs

depth :: WFF -> Numb
depth n = case n of
                T -> one
                F -> one
                Neg x -> add one (depth x)
                Conj x y -> add one (bigger (depth x) (depth y))
                Disj x y -> add one (bigger (depth x) (depth y))
