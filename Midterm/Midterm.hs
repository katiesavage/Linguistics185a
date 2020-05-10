module Midterm where

import ProbSLG
import Helpers

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
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
buildProbSLG :: Ord a => Corpus a -> ProbSLG a
buildProbSLG corpus = 
    let s = symP (concat (map (take 1) corpus)) in
        let f = symP (map (\q -> (last q)) corpus) in
            let tr = trP corpus in
                ProbSLG (s, f, tr)

symP :: Ord a => [a] -> [(a, Double)]
symP lst = 
    let totalsy = length lst in
        let frq = frequencies lst in
            map (\(q,n) -> (q,(divide n totalsy))) frq

trP :: Ord a => Corpus a -> [(a, a, Double)]
trP corpus = 
    let trs = concat (map (\q -> bigrams q) corpus) in --list of bigrams/transitions within corpus
        let frq = frequencies trs in --frequency of each transition
            (map (\((q1,q2),n) -> (q1,q2,
                                    (divide n (length ((filter (\(q1',q2') -> q1'==q1) trs))))))
                                frq
            )

-------------------------------------------------------------------------------
-- Problem 3:
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.
sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = [filter (\q -> not (ispunctuation (getWord q))) 
            , filter (\q -> not (ispunctuation (getTag q)))] --"words" keyword

ispunctuation :: String -> Bool
ispunctuation sym = 
    case sym of
        "'" -> True
        "''" -> True
        "(" -> True
        ")" -> True
        "*" -> True
        "," -> True
        ":" -> True
        "``" -> True
        _ -> False


posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG corpus = --want to turn take the rhs values of the tagged word tuples
    let ttot = map (getTag) in --tuple to tag
        buildProbSLG (map (\q -> ttot q) corpus)

twProbSLG :: Corpus TaggedWord -> ProbSLG TaggedWord
twProbSLG corpus = buildProbSLG corpus 
                            -- ProbSLG ( [(TaggedWord, Double)]      -- Starting symbols.
                            --  , [(TaggedWord, Double)]      -- Final symbols.
                            --  , [(TaggedWord, TaggedWord, Double)]  -- Transitions.
                            --  )

tag :: Corpus TaggedWord -> String -> [(Sentence TaggedWord, Double)]
tag corpus str =
    -- String -> [String]
    let s = words str in
    let tr = bigrams s in 
        let pslg = twProbSLG corpus in 
        let ProbSLG (ss, fs, ts) = pslg in
            let starts = filter (\(TaggedWord(q,q'),d) -> q == (head s)) ss in
                sentence_starter pslg tr starts
                
sentence_starter :: ProbSLG TaggedWord -> [(String, String)] -> [(TaggedWord, Double)] -> [(Sentence TaggedWord, Double)]
sentence_starter pslg tr starts = 
    case starts of
        [] -> []
        x:xs -> let (TaggedWord(w,pos),d')= x in
            sentence_maker pslg tr [(fst x)] pos (snd x) ++ sentence_starter pslg tr xs 

sentence_maker :: ProbSLG TaggedWord -> [(String, String)] -> Sentence TaggedWord -> String -> Double -> [(Sentence TaggedWord, Double)]
sentence_maker pslg tr sentence pos prob =  --start is the w,tag of the starting word, prob is current count of probability --ending form must be (Sentence TaggedWord, double)
    let ProbSLG (ss,fs,ts) = pslg in
        case tr of
            --end of sentence-> return sentence, total_prob
            [] -> let (TaggedWord (tq,tq')) = (last sentence) in
                    let finals = filter (\(TaggedWord (q,q'),d')-> q==tq) fs in
                        case finals of
                            [] -> [] --not an acceptable final symbol
                            _ -> [(sentence,(prob * head(map (\(tw,d)->d) finals)))]
            --more sentence->recursively pursue different iterations of sentence from this point
            (x,y):xs -> let nextsteps = filter (\(TaggedWord (w1,p1),TaggedWord (w2,p2),_)-> (w1,w2,p1) == (x,y,pos)) ts in  --filter for transitions with x bigram
                case nextsteps of
                    [] -> [] --maybe type?? none and just--all acceptable strings already explored (ie current transition not possible)
                    (a,b,d):as -> let TaggedWord(w,pos') = b in
                                    (sentence_maker pslg xs (sentence++[b]) pos' (prob*d)) 
                                    ++ (sentence_maker (ProbSLG (ss,fs, (filter (\q -> q/=(a,b,d)) ts))) tr sentence pos prob)

-- --tag attempt1
-- tag :: Corpus TaggedWord -> String -> [(Sentence TaggedWord, Double)]
-- tag corpus str = 
--     -- break sentence into usable units
--     let s = words str in
--     --let tr = bigrams s in
--         -- train corpus using posProbSLG -- get appropriate POS relations
--         --let ProbSLG pslg = posProbSLG corpus in
--         --let (ss, fs, ts) = pslg in
--             -- possible first and last WORDS given corpus
--             let firsts = concat (map (take 1) corpus) in
--             --let lasts = concat (map (\q -> (last q)) corpus) in
--                 -- get all possible firsts (WORDS, pos) matching the head of the sentence of interest
--                 let pfirsts = filter (\(TaggedWord (q,q')) -> q == (head s)) firsts in  
--                         sentence_starter corpus s pfirsts
--         -- use valP given pos and sentence
--         --valP pslg s

-- sentence_starter :: Corpus TaggedWord -> [String] -> [TaggedWord] -> [(Sentence TaggedWord, Double)]
-- sentence_starter corpus s firsts = 
--     let pslg = posProbSLG corpus in
--         case firsts of
--             [] -> []
--             x:xs -> let startp = 
--                 sentence_maker pslg s x : sentence_starter corpus s xs

tagBest :: Corpus TaggedWord -> String -> Sentence TaggedWord
tagBest corpus str = 
    let options = tag corpus str in
        compare_tb options (head options)

compare_tb :: [(Sentence TaggedWord, Double)] -> (Sentence TaggedWord, Double) -> Sentence TaggedWord
compare_tb options highest = 
    case options of
        [] -> fst highest
        x:xs -> let (tw,d) = x in
                let (tw',d') = highest in
                --let new_highest = (map (\(TaggedWord(w,p),d) -> d) x) > (map (\(TaggedWord(w,p),d) -> d) highest) in
                    let new_highest = d > d' in
                        case new_highest of
                            True -> compare_tb xs x
                            False -> compare_tb xs highest