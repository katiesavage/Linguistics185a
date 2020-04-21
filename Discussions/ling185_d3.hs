--implementation of map function
applyToAll :: (a->a)->[a]->[a]
applyToAll x list = case list of
                         [] -> []
                         (h:t) -> (x h) : (applyToAll x t)

--suffixes
suffixes :: [a]->[[a]]
suffixes list = case list of
                     [] -> [[]]
                     (h:t) -> (h:t):(suffixes t)

--beforeAndAfter
beforeAndAfter :: (a->b)->[a]->[(a,b)]
beforeAndAfter f [] = []
beforeAndAfter f (x:xs) = (x, f x) : (beforeAndAfter f xs)

--inverseMap
inverseMap::[(a->b)]->a->[b]
inverseMap [] x = [] 
inverseMap (f:fs) x = f x : (inverseMap fs x)

--countEvens
countEvens::Integral a -> [[a]] -> [Int]
countEvens a = map (\x -> length (filter (\n -> n `mod` 2 == 0) x)) a
