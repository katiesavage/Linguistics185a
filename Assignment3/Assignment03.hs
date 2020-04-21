module Assignment03 where

import RegEx
import SLG

import Data.List (nub)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

bigrams :: [a] -> [(a, a)]
bigrams list = case list of 
    [] -> []
    x:xs -> case xs of 
                [] -> []
                y:ys -> (x,y):(bigrams xs)

pretty :: (Eq a) => [(a, a)] -> [a]
pretty bg = case isChained bg of
    False -> []
    True -> case bg of
        [] -> []
        (x,y):[] -> x:[y]
        (x,y):xs -> x:(pretty xs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--look at third list in slg list to get bigrams (list of tuples)
fhelper :: [(sy,sy)] -> [sy]
fhelper lst = case lst of 
    [] -> []
    (x,y):xs -> y:(fhelper xs)

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows (_,_,lst) sym = nub (fhelper (filter ((==sym).fst) lst))
    
phelper :: [(sy,sy)] -> [sy]
phelper lst = case lst of 
    [] -> []
    (x,y):xs -> x:(phelper xs)

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes (_,_,lst) sym = nub (phelper (filter ((==sym).snd) lst))

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the"],["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very"],["very","very","very"],["very","very","fat"],
--    ["very","fat","cat"],["very","very"],["very","fat"]]

forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward slg 0 sym = [[sym]]
forward slg n sym = 
    let nxtlvl = follows slg sym in
        nub (forward' slg n nxtlvl [sym] ++ forward slg (n-1) sym)

forward' :: (Eq sy) => SLG sy -> Int -> [sy] -> [sy] -> [[sy]]
forward' slg n list tmp = case list of
    [] -> [tmp]
    (x:xs) -> case n of
        0 -> [tmp]
        _ -> (forward' slg (n-1) (follows slg x) (tmp++[x])) ++ (forward' slg n xs tmp)

-- MORE EXAMPLE USAGE:
-- backward g2 1 "cat"
-- => [["cat"],["the","cat"],["fat","cat"]]
-- backward g2 2 "very"
-- => [["very"],["the","very","very"],["very","very","very"],["the","very"],
--    ["very","very"]]

backward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
backward slg 0 sym = [[sym]]
backward slg n sym = 
    let nxtlvl = precedes slg sym in
        nub (backward' slg n nxtlvl [sym] ++ backward slg (n-1) sym)

backward' :: (Eq sy) => SLG sy -> Int -> [sy] -> [sy] -> [[sy]]
backward' slg n list tmp = case list of
    [] -> [tmp]
    (x:xs) -> case n of
        0 -> [tmp]
        _ -> (backward' slg (n-1) (precedes slg x) ([x]++tmp)) ++ (backward' slg n xs tmp)

generates :: (Eq sy) => SLG sy -> Int -> [[sy]]
generates slg n = 
    let (fst,lst,_) = slg in
        let flist = forward slg n (head fst) in
            filter (`elem` flist) (backward slg n (head lst)) --(forward slg n first)++(backward slg n last)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

occurrences :: Int -> (RegEx a) -> (RegEx a)
occurrences 0 r = One
occurrences n r = (Concat r (occurrences (n-1) r)) 


optional :: (RegEx a) -> (RegEx a)
optional r = Alt Zero r
