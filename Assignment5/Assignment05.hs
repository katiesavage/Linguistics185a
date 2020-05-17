module Assignment05 where

import FSA

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fwdProb :: (Ord st, Ord sy)
        => ProbFSA st sy
        -> [sy]
        -> st
        -> Double
fwdProb pfsa str q = fwdProb' pfsa (reverse str) q

fwdProb' :: (Ord st, Ord sy)
        => ProbFSA st sy
        -> [sy]
        -> st
        -> Double
fwdProb' pfsa str q =
    case str of
        []     -> initProb pfsa q
        (x:xs) ->
            sum $ map (\q' -> trProb pfsa q' x q * fwdProb' pfsa xs q') states
    where
        states = allStatesPFSA pfsa
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

unionFSAs :: (Ord sy)
          => EpsAutomaton Int sy
          -> EpsAutomaton Int sy
          -> EpsAutomaton Int sy
unionFSAs efsa1 efsa2 = 
    EpsAutomaton (1
                , f1++f2
                , (newtr:tr1) ++ (newtr2:tr2)
                )
    where
        efsa1_st = allStatesEFSA efsa1
        efsa11 = ensureUnused [1] efsa1
        efsa22 = ensureUnused (1:efsa1_st) efsa2
        EpsAutomaton (s1,f1,tr1) = efsa11
        EpsAutomaton (s2,f2,tr2) = efsa22
        newtr = (1, Nothing, s1)
        newtr2 = (1, Nothing, s2)

concatFSAs :: (Ord sy)
            => EpsAutomaton Int sy
            -> EpsAutomaton Int sy
            -> EpsAutomaton Int sy
concatFSAs efsa1 efsa2 =
        concatFSAs' efsa1 efsa2
    where
        efsa1_st = allStatesEFSA efsa1
        efsa22 = ensureUnused efsa1_st efsa2 --make sure there are no repeated states between the two

concatFSAs' :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy -> EpsAutomaton Int sy
concatFSAs' efsa1 efsa2 = 
    case f1 of
        [] ->   EpsAutomaton (s1
                            , f2
                            , tr1 ++ tr2
                            )
        x:xs -> let efsa1_n = (EpsAutomaton (s1,xs,(x, Nothing, s2):tr1)) in
                    concatFSAs' efsa1_n efsa2
    where
        efsa1_st = allStatesEFSA efsa1
        EpsAutomaton (s1,f1,tr1) = efsa1
        EpsAutomaton (s2,f2,tr2) = efsa2

starFSA :: (Ord sy)
        => EpsAutomaton Int sy
        -> EpsAutomaton Int sy
starFSA efsa = 
        starFSA' efsa2 s f
    where
        efsa2 =  ensureUnused [1] efsa
        EpsAutomaton (s,f,tr) = efsa2

starFSA' :: (Ord sy) => EpsAutomaton Int sy -> Int -> [Int] -> EpsAutomaton Int sy
starFSA' efsa s1 f1 = 
    case f of
        [] ->
            EpsAutomaton (1
                        , newstart:f1
                        , (fsttr ++ tr)
                        )
        x:xs -> let nefsa = (EpsAutomaton (1,xs,(x, Nothing, s):tr)) in
            starFSA' nefsa s1 f1
    where
        EpsAutomaton (s,f,tr) = efsa
        newstart = 1
        fsttr = [(newstart, Nothing, s1)]



reToFSA :: (Ord sy)
        => RegEx sy
        -> EpsAutomaton Int sy
reToFSA (Lit a) = EpsAutomaton (0
                                , [1]
                                , [(0, Just a, 1)]
                                ) 
reToFSA (Alt r1 r2) = 
    let r1efsa = reToFSA r1 in
        let r2efsa = reToFSA r2 in
            unionFSAs r1efsa (ensureUnused (allStatesEFSA r1efsa) r2efsa)
reToFSA (Concat r1 r2) = 
    let r1efsa = reToFSA r1 in
        let r2efsa = reToFSA r2 in
            concatFSAs r1efsa (ensureUnused (allStatesEFSA r1efsa) r2efsa)
reToFSA (Star r) = starFSA (reToFSA r)
reToFSA Zero = EpsAutomaton (0
                            , []
                            , []
                            ) 
reToFSA One = EpsAutomaton (0
                            , [1]
                            , [(0, Nothing, 1)]
                            )
