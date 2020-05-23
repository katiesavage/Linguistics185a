module Assignment06 where

import CFG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

checkT :: [Symbol nt t] -> Bool
checkT lst = case lst of 
        [] -> True
        x:xs -> case x of 
            T x -> checkT xs
            NT x -> False 

terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly lst = 
    case checkT lst of
        False -> Nothing
        True -> Just (map (\(T x) -> x) lst)

leaves :: Tree nt t -> [t]
leaves tree = 
    case tree of
        Leaf nt t -> [t] 
        NonLeaf nt ltree rtree -> leaves ltree ++ leaves rtree

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList tree = 
    case tree of
        -- terminal rule
        Leaf nt t -> [TerminalRule nt t]
        -- nonterminal rule
        NonLeaf nt tree1 tree2 -> (NonterminalRule nt (root tree1, root tree2)) 
                                                    : treeToRuleList tree1 
                                                    ++ treeToRuleList tree2

--make tree, make symbol list from tree (leaves), check symbol list with terminalsOnly
ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree rulelst =   
    let (NonterminalRule nt (nt2,nt3)):rest = rulelst in   
        Just (ruleListMaker nt rulelst)    

ruleListMaker :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> (Tree nt t)
ruleListMaker sym rulelst = 
    case rulelst of
        (TerminalRule nt t):r -> if nt==sym then (Leaf nt t)
                                    else ruleListMaker sym r
        (NonterminalRule nt (nt2, nt3)):r -> if nt==sym then NonLeaf nt (ruleListMaker nt2 r) (ruleListMaker nt3 r)
                                    else ruleListMaker sym r

treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost = undefined

rewriteLeftmost :: (Eq nt, Eq t)
                => [RewriteRule nt t]
                -> [Symbol nt t]
                -> [[Symbol nt t]]
rewriteLeftmost = undefined

derivableFrom :: (Eq nt, Eq t)
              => [Symbol nt t]
              -> [RewriteRule nt t]
              -> Int
              -> [[t]]
derivableFrom = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

derivable :: (Eq nt, Eq t) => CFG nt t -> Int -> [[t]]
derivable (start , rules) n = derivableFrom [NT start] rules n
