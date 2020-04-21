data WFF = T             -- true
         | F             -- false
         | Neg WFF       -- NOT p
         | Conj WFF WFF  -- p AND q
         | Disj WFF WFF  -- p OR q
         deriving Show

denotation :: WFF -> Bool
denotation wff = case wff of 
   T -> True
   F -> False
   Neg x -> (denotation x) == False
   Conj x y -> (True == (denotation x)) && (True == (denotation y))
   Disj x y -> (True == (denotation x)) || (True == (denotation y))
