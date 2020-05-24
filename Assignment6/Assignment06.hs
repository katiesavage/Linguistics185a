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
        case (ruleListCheck nt rulelst) of
            False -> Nothing
            True -> Just (ruleListMaker nt rulelst)    

ruleListCheck :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> Bool
ruleListCheck sym rulelst = 
    case rulelst of
        [] -> False
        (TerminalRule nt t):r -> if nt==sym then True
                                    else ruleListCheck sym r
        (NonterminalRule nt (nt2, nt3)):r -> if nt==sym then ((ruleListCheck nt2 r) && (ruleListCheck nt3 r))
                                    else ruleListCheck sym r

--assumes correct tree
ruleListMaker :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> (Tree nt t)
ruleListMaker sym rulelst = 
    case rulelst of
        (TerminalRule nt t):r -> if nt==sym then (Leaf nt t)
                                    else ruleListMaker sym r
        (NonterminalRule nt (nt2, nt3)):r -> if nt==sym then (NonLeaf nt (ruleListMaker nt2 r) (ruleListMaker nt3 r))
                                    else ruleListMaker sym r

-- (NonLeaf 1 
    -- (NonLeaf 2 
        --(Leaf 4 'a') 
        --(Leaf 5 'b')) 
    --(Leaf 3 'c'))
treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = 
    case tree of
        -- Leaf
        Leaf nt t -> [NT nt] : [[T t]]
        -- NonLeaf node
        NonLeaf nt lt rt -> 
            let lderiv = treeToDerivation lt in
                let rderiv = treeToDerivation rt in
                    [[NT nt]] 
                    ++ map (\q -> q ++ (head rderiv)) lderiv 
                    ++ map (\q' -> (last lderiv) ++ q') (tail rderiv)
   
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost [] = Nothing
splitAtLeftmost (x:xs) =
    case x of
        NT y    -> Just ([], y, xs)
        _       -> case splitAtLeftmost xs of
                        Nothing         -> Nothing
                        Just (ys, y, zs)   -> Just (x:ys, y, zs)

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
