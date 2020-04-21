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

g1 :: SLG SegmentCV
g1 = ( [C]                       -- Starting symbols.
     , [V]                       -- Final symbols.
     , [(C, V), (V, C), (V, V)]  -- Transitions.
     )

g2 :: SLG String
g2 = ( ["the"]
     , ["cat"]
     , [ ("the", "cat"), ("the", "very"), ("the", "fat")
       , ("very", "very"), ("very", "fat"), ("fat", "cat") ]
     )

g3 :: SLG Int
g3 = ( [1, 2]
     , [1, 2]
     , [ (1, 1), (2, 2) ]
     )

-------------------------------------------------------------------------------
-- Helper functions.
-------------------------------------------------------------------------------

isChained :: (Eq a) => [(a, a)] -> Bool
isChained [] = True
isChained (x:[]) = True
isChained (x:y:rest) = (snd x == fst y) && isChained (y:rest)
