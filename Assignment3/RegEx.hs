module RegEx where

-------------------------------------------------------------------------------
-- Syntax of regular expressions.
-------------------------------------------------------------------------------

data RegEx a = Lit a
             | Alt (RegEx a) (RegEx a)
             | Concat (RegEx a) (RegEx a)
             | Star (RegEx a)
             | Zero
             | One
             deriving Show 

-------------------------------------------------------------------------------
-- Semantics of regular expressions.
-------------------------------------------------------------------------------

denotation :: RegEx a -> [[a]]
denotation (Lit a) = [ a : [] ]
denotation (Alt r1 r2) = denotation r1 ++ denotation r2
denotation (Concat r1 r2) =
    [ u ++ v | u <- (denotation r1), v <- (denotation r2) ]
denotation (Star r) =
    [] : [ u ++ v | u <- (denotation r), v <- denotation (Star r) ]
denotation Zero = []
denotation One = [[]]
