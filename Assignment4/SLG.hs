module SLG where

-------------------------------------------------------------------------------
-- Type for SLGs.
-------------------------------------------------------------------------------

type SLG sy = ( [sy]        -- Start symbols.
              , [sy]        -- Final symbols.
              , [(sy, sy)]  -- Transitions, list of pairs.
              )

-------------------------------------------------------------------------------
-- Example SLGs.
-------------------------------------------------------------------------------

data SegmentCV = C
               | V
               deriving (Show, Eq, Ord)

slg1 :: SLG SegmentCV
slg1 = ( [C]                       -- Starting symbols.
       , [V]                       -- Final symbols.
       , [(C, V), (V, C), (V, V)]  -- Transitions.
       )

slg2 :: SLG String
slg2 = ( ["the"]
       , ["cat"]
       , [ ("the", "cat"), ("the", "very"), ("the", "fat")
       , ("very", "very"), ("very", "fat"), ("fat", "cat") ]
       )

slg3 :: SLG Int
slg3 = ( [1, 2]
       , [1, 2]
       , [ (1, 1), (2, 2) ]
       )

-------------------------------------------------------------------------------
-- Recognize function.
-------------------------------------------------------------------------------

recognizeSLG :: (Ord sy)
             => SLG sy   -- A function from SLG grammars with alphabet sy
             -> [sy]     -- to functions from lists of sy-expressions
             -> Bool     -- to booleans.

-- Bigrams are unable to generate the empty string. The _ is a wildcard, which
-- matches everything and performs no binding. It's appropriate here because
-- the inability to generate the empty strings extends to all SLGs.
recognizeSLG _ [] = False

-- Main part of the function. The first argument is deconstructed using pattern
-- matching that binds the tuple members to variables.
recognizeSLG (start, final, trans) str =
       elem (head str) start      -- Is the first char of str in S?
    && elem (last str) final      -- Is the last char of str in F?
    && and (map inTrans bigrams)  -- Are all the bigrams in T?
  where
    bigrams = zip str (tail str)  -- Creates a list of the bigrams in str.
    inTrans x = elem x trans      -- Helper fn: is x in T?

-------------------------------------------------------------------------------
-- Helper functions.
-------------------------------------------------------------------------------

isChained :: (Eq a) => [(a, a)] -> Bool
isChained [] = True
isChained (x:[]) = True
isChained (x:y:rest) = (snd x == fst y) && isChained (y:rest)
