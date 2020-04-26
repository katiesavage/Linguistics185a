module Assignment04 where

import FSA
import SLG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fsa2 :: Automaton Int SegmentCV
fsa2 = ( 1
        , [3]
        , [ (1, C, 1), (1, V, 1), (1, C, 2)
            , (2, C, 2), (2, V, 2), (2, C, 3)
            , (3, C, 3), (3, V, 3)
        ]
     )

fsa3 :: Automaton Int SegmentCV
fsa3 = ( 1
        , [2,5]
        , [ (1, C, 2), (1, V, 3)
            , (2, C, 1), (2, V, 4), (3, V, 1), (3, C, 4)
            , (4, V, 5)
            , (5, C, 1)
        ]
     )

fsa4 :: Automaton Int SegmentCV
fsa4 = ( 1
        , [4]
        , [ (1, C, 1), (1, V, 1), (1, C, 2)
            , (2, C, 3), (2, V, 3)
            , (3, C, 4), (3, V, 4)
        ]
     )

fsa5 :: Automaton Int SegmentPKIU
fsa5 = ( 1
        , [1,2,3]
        , [ (1, P, 1), (1, K, 1), (1, I, 2), (1, U, 3)
            , (2, P, 2), (2, K, 2), (2, I, 2), (2, WB, 1)
            , (3, P, 3), (3, K, 3), (3, U, 3), (3, WB, 1)
        ]
     )

fsa6 :: Automaton Int SegmentPKIU
fsa6 = ( 1
        , [1,3]
        , [ (1, P, 1), (1, K, 1), (1, I, 1), (1, P, 2)
            , (2, P, 2), (2, K, 2), (2, I, 2), (2, U, 3)
            , (3, P, 3), (3, K, 3), (3, I, 3), (3, U, 3)
        ]
     )

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (s, f, t) = 
    let ss = ExtraState in --start state
        let ssym = getsym s ss in --start symbols transitions
            let trn = ssym ++ tfunction t in --rest of transitions
                let fs = undefined in
                    (ss,fs,trn) --start state, final state, transitions

getsym :: [sy] -> ConstructedState sy -> [(ConstructedState sy, sy, ConstructedState sy)]
getsym s ss = case s of
    [] -> []
    (x:xs) -> (ss, x, StateForSymbol x) : getsym xs ss

tfunction :: [(sy, sy)] -> [(ConstructedState sy, sy, ConstructedState sy)]
tfunction t = case t of
    [] -> []
    (s1,s2):r -> (StateForSymbol s1, s2, StateForSymbol s2) : (tfunction r)

-- tfunction :: [(sy, sy)] -> [(ConstructedState sy, sy, ConstructedState sy)] -> [(ConstructedState sy, sy, ConstructedState sy)]
-- tfunction t fsat = let (st,sym,st2):rest = fsat in
--     case t of
--         [] -> []
--         ((a,b):r) && (a==sym) -> (st2,sym,StateForSymbol sym): tfunction r fsat
--         _ -> tfunction t rest

-- tfunction t fsat = 
--     let filterfsa = filter ((==sym).snd) fsat in --let (st,sym,st2):rest = fsat in
--         case t of
--             [] -> []
--             (a,b):r -> case a of
--                 a `elem` filterfsa -> (st2,sym,StateForSymbol sym): tfunction r fsat
--             _ -> tfunction t rest

-- tfunction ((a,b):r) fsat = case fsat of --((st,sym,st2):rest) = undefined 
--     [] -> []
--     (st,sym,st2):rest when a=sym -> (st2,sym,ConstructedState sym): tfunction r fsat
--     _ -> tfunction r fsat

-- tfunction :: [(sy, sy)] -> [(ConstructedState sy, sy, ConstructedState sy)] -> [(ConstructedState sy, sy, ConstructedState sy)]
-- tfunction t fsat = --let fsat = (st,sym,st2):rest in
--     case t of --t = list of tuples
--         [] ->
--         ((a,b):xs) -> case fsat of
--             [] ->
--             (st,sym,st2):rest when a=b=sym -> (st2,a,st2): tfunction xs rest
--             (st,sym,st2):rest when a=sym -> ()