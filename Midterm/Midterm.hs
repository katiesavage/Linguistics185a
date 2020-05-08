module Midterm where

import ProbSLG
import Helpers

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
import Data.List (nub)
-------------------------------------------------------------------------------
-- Problem 1:
-------------------------------------------------------------------------------
fhelper :: [(sy,sy,Double)] -> [(sy,Double)]
fhelper lst = case lst of 
    [] -> []
    (x,y,z):xs -> (y,z):(fhelper xs)

follows :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
follows (ProbSLG pslg) sym = 
    let (ss,fs,t) = pslg in --start state, final state, transitions
        fhelper (filter (\(a,_,_) -> a == sym) t) 
-------------------------------------------------------------------------------
phelper :: [(sy,sy,Double)] -> [(sy,Double)]
phelper lst = case lst of 
    [] -> []
    (x,y,z):xs -> (x,z):(phelper xs)

precedes :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
precedes (ProbSLG pslg) sym = 
    let (ss,fs,t) = pslg in --start state, final state, transitions
        phelper (filter (\(_,b,_) -> b == sym) t) 
-------------------------------------------------------------------------------
startp :: Ord sy => [(sy, Double)] -> sy -> Double
startp ss s = 
    let t = (filter (\(x,_) -> x == s) ss) in
        case t of
            [] -> 0
            _ -> snd(head t)

finalp :: Ord sy => [(sy, Double)] -> [sy] -> Double
finalp fs s = 
    case s of
        x:[] -> startp fs x
        x:xs -> finalp fs xs

trp :: Ord sy => [(sy, sy, Double)] -> [sy] -> Double
trp t s = 
    case s of
        x:[] -> 1
        x: xs -> case xs of
            [] -> 1
            y: ys -> let matcht = (filter (\(a,b,_) -> (a,b) == (x,y)) t) in
                case matcht of
                    [] -> 0
                    _ -> let (_,_,p) = head matcht in
                        p * (trp t xs)

valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP (ProbSLG pslg) s = --the probability that s is a string 
    let (ss,fs,t) = pslg in --start state, final state, transitions
        --check start sym
        let ssprob = startp ss (head s) in --probability of start
            --check final sym
            let fsprob = finalp fs s in --probability of final
                --go through transitions
                let tprob = trp t s in 
                    ssprob * fsprob * tprob
-------------------------------------------------------------------------------
valP' :: Ord sy => ProbSLG sy -> [sy] -> Double
valP' (ProbSLG pslg) s = 
    let (ss,fs,t) = pslg in
        trp t s
-------------------------------------------------------------------------------
-- Problem 2:
-------------------------------------------------------------------------------
-- Returns all the states mentioned anywhere in a ProbFSA.
allStates :: Ord sy => ProbSLG sy -> [sy]
allStates (ProbSLG (starts, ends, trans)) =
    nub $ concat [ map (\(q, _) -> q) starts
                 , map (\(q, _) -> q) ends
                 , map (\(q, _, _) -> q) trans
                 , map (\(_, q, _) -> q) trans
                 ]

-- buildProbSLG :: Ord a => Corpus a -> ProbSLG a
-- buildProbSLG [] = ProbSLG ([], [], [])

-- buildProbSLG corpus = 
--     let initP = *get starting values* in
--         let finP = *get final values* in
--             let trP = *get transition values* in
--                 ProbSLG (initP, finP, trP)

-- A potentially helpful starting point:
-- buildProbSLG corpus = ProbSLG ([], [], [])

-------------------------------------------------------------------------------
-- Problem 3:
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.
sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = []

posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG = undefined

tag :: ProbSLG String -> String -> [(Sentence TaggedWord, Double)]
tag = undefined

tagBest :: ProbSLG String -> String -> Sentence TaggedWord
tagBest = undefined
