module FSA where

import Data.List (nub)

-------------------------------------------------------------------------------
-- Type for FSA.
-------------------------------------------------------------------------------

type Automaton states alphabet =
    (states, [states], [(states, alphabet, states)])

targets :: (Ord s, Ord a) => [(s, a, s)] -> s -> a -> [s]
targets [] q x = []
targets (head:tail) q x =
    if   q1 == q && y == x
    then q2 : (targets tail q x) -- Save and recurse.
    else targets tail q x -- Recurse, but don't save.
  where
    (q1, y, q2) = head -- Unpack the first tuple.

hat :: (Ord s, Ord a) => [(s, a, s)] -> s -> [a] -> [s]
hat delta q [] = [q]
hat delta q (x:u) =
    nub $ concat $ -- Big union and remove duplicates.
        map
            (\q' -> hat delta q' u) -- Recurse on target states.
            (targets delta q x) -- Get possible target states.

recognize :: (Ord s, Ord a) => Automaton s a -> [a] -> Bool
recognize (start, ends, delta) u =
    or $ -- Big disjunction: something in list must be True
        map
            (\q -> elem q ends) -- Check if q is an end state.
            (hat delta start u) -- Get all states that would emit u.

-------------------------------------------------------------------------------
-- Extra alphabets.
-------------------------------------------------------------------------------

data SegmentPKIU = P | K | I | U | WB deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Extra state types.
-------------------------------------------------------------------------------

data ConstructedState sy = ExtraState
                         | StateForSymbol sy
                         deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Helper functions.
-------------------------------------------------------------------------------

transitions :: (Automaton s a) -> [(s, a, s)]
transitions (_, _, x) = x
