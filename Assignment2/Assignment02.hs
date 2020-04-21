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
times E b = E
times (S a) b = add (times a b) b  

equal :: Numb -> Numb -> Bool
equal E E = True
equal E (S b) = False
equal (S a) E = False
equal (S a) (S b) = equal a b 

bigger :: Numb -> Numb -> Numb
bigger E b = b
bigger a E = a
bigger (S a) (S b) = S (bigger a b) 

count :: (a -> Bool) -> [a] -> Numb
count f ls = case ls of [] -> E
                        (x:xs) -> case f x of True -> S (count f xs)
                                              False -> count f xs

remove :: (a -> Bool) -> [a] -> [a]
remove f l = case l of [] -> []
                       (x:xs) -> case f x of True -> remove f xs
                                             False -> x:(remove f xs)

prefix :: Numb -> [a] -> [a]
prefix n list = case list of [] -> list
                             (x:xs) -> case n of E -> []
                                                 (S y) -> x:(prefix y xs)

depth :: WFF -> Numb
depth wff = case wff of T -> (S E)
                        F -> (S E)
                        Neg a -> S (depth a)
                        Conj a b -> S (bigger (depth a) (depth b))
                        Disj a b -> S (bigger (depth a) (depth b))
