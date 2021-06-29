module Assignment06 where

import CFG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----  Question 1
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- TerminalsOnly Function -----

terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly symbolList = result
    where
        result = case (null nonTerminalList) of False -> Nothing
                                                True -> Just (map (\x -> (case x of (T y) -> y)) symbolList)
        nonTerminalList = filter (\x -> (case x of (NT y) -> True
                                                   (T y) -> False)) symbolList



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- Leaves Function -----

leaves :: Tree nt t -> [t]
leaves (Leaf a b) = [b]
leaves (NonLeaf a b c) = leaves b ++ leaves c







-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----  Question 2
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- TreeToRuleList Function -----

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList (Leaf a b) = [TerminalRule a b]
treeToRuleList (NonLeaf a b c) = [NonterminalRule a (root b, root c)] ++ treeToRuleList b ++ treeToRuleList c



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- RuleListToTree Function -----

ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree ruleList = case (hasBalancedTotalNonTerm ruleList) of True -> case (hasValidLeftmostDerivation ruleList) of True -> Just (createTreeRecur ruleList)
                                                                                                                           False -> Nothing
                                                                     False -> Nothing


----- RuleListToTree Helper Functions -----

createTreeRecur :: (Eq nt, Eq t) => [RewriteRule nt t] -> (Tree nt t)
createTreeRecur (x:ys) = case x of (NonterminalRule a (b, c)) -> (NonLeaf a ((createTreeRecur (ys))) ((createTreeRecur (drop (1 + dropNum (ys)) (ys)))))
                                   (TerminalRule d e) -> (Leaf d e)


dropNum :: (Eq nt, Eq t) => [RewriteRule nt t] -> Int
dropNum x = case head(x) of (NonterminalRule aa (bb, cc)) -> 2 + dropNum (drop 1 x) + dropNum (drop (2 + dropNum(drop 1 x)) x)
                            (TerminalRule dd ee) -> 0


hasValidLeftmostDerivation :: (Eq nt, Eq t) => [RewriteRule nt t] -> Bool
hasValidLeftmostDerivation [x] = case x of (NonterminalRule a (b, c)) -> False
                                           (TerminalRule d e) -> True
hasValidLeftmostDerivation (x:ys) = case x of (NonterminalRule a (b, c)) -> if (null (drop (1 + dropNum (ys)) ys)) == True
                                                                            then False
                                                                            else if (b == lhs(head(ys)) && c == lhs(head(drop (1 + dropNum (ys)) ys)))
                                                                                 then hasValidLeftmostDerivation (ys)
                                                                                 else False
                                              (TerminalRule d e) -> hasValidLeftmostDerivation (ys)


hasBalancedTotalNonTerm :: (Eq nt, Eq t) => [RewriteRule nt t] -> Bool
hasBalancedTotalNonTerm xs = case (nTotal == ((2 * nNonTerminal) + 1)) of True -> True
                                                                          False -> False
    where
        nTotal = length (xs)
        nNonTerminal = length (filter (\x -> case x of (NonterminalRule a (b, c)) -> True
                                                       (TerminalRule a b) -> False) xs)



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- TreeToDerivation Function -----

treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation (Leaf a b) = [[NT a], [T b]]
treeToDerivation (NonLeaf a b c) = [[NT a]] 
                                   ++ [head (treeToDerivation b) ++ head(treeToDerivation c)]
                                   ++ map (\x -> x ++ head(treeToDerivation c)) (tail (treeToDerivation b))
                                   ++ map (\y -> last(treeToDerivation b) ++ y) (tail (treeToDerivation c))







-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----  Question 3
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- SplitAtLeftMost Function -----

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])

splitAtLeftmost [] = Nothing
splitAtLeftmost (x:xs) = case x of (NT key) -> Just ([], key, xs)
                                   (T key) -> case (splitAtLeftmost xs) of Just (a, b, c) -> Just ((x:a), b, c)
                                                                           Nothing -> Nothing



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- RewriteLeftMost Function -----

rewriteLeftmost :: (Eq nt, Eq t)
                => [RewriteRule nt t]
                -> [Symbol nt t]
                -> [[Symbol nt t]]

rewriteLeftmost [] xs = []
rewriteLeftmost (r:rs) xs =
    case (splitAtLeftmost xs) of
        Nothing                   -> [xs]
        Just (alpha, beta, gamma) -> case r of (NonterminalRule x (y, z)) -> case (x == beta) of True  -> [alpha ++ [NT y, NT z] ++ gamma] ++ rewriteLeftmost rs xs
                                                                                                 False -> [] ++ rewriteLeftmost rs xs
                                               (TerminalRule x y)         -> case (x == beta) of True -> [alpha ++ [T y] ++ gamma] ++ rewriteLeftmost rs xs
                                                                                                 False -> [] ++ rewriteLeftmost rs xs



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

----- DerivableFrom Function -----

derivableFrom :: (Eq nt, Eq t)
              => [Symbol nt t]
              -> [RewriteRule nt t]
              -> Int
              -> [[t]]
derivableFrom sym r num = map (\x -> (map (\y -> (case y of (T z) -> z)) x)) (derivableFromRecur sym r num)



----- DerivableFrom Helper Function -----

derivableFromRecur :: (Eq nt, Eq t)
                   => [Symbol nt t]
                   -> [RewriteRule nt t]
                   -> Int
                   -> [[Symbol nt t]]
derivableFromRecur sym r 0 = []
derivableFromRecur sym r num = concat (map (\x -> derivableFromRecur x r (num-1)) devwithNT) ++ devwithoutNT
    where
        devwithNT = filter (\x -> terminalsOnly x == Nothing) (rewriteLeftmost r sym)               -- Needs Recursion
        devwithoutNT = filter (\x -> terminalsOnly x /= Nothing) (rewriteLeftmost r sym)            -- Final Form



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

derivable :: (Eq nt, Eq t) => CFG nt t -> Int -> [[t]]
derivable (start , rules) n = derivableFrom [NT start] rules n























-- Some Test Cases

--treeToDerivation (NonLeaf 1 (NonLeaf 2 (NonLeaf 8 (Leaf 9 'f')(NonLeaf 10 (Leaf 11 'o')(Leaf 12 'p'))) (Leaf 5 'b')) (NonLeaf 3(Leaf 6 'c')(Leaf 7 'd')))
--[[NT 1],[NT 2,NT 3],[NT 8,NT 5,NT 3],[NT 9,NT 10,NT 5,NT 3],[T 'f',NT 10,NT 5,NT 3],[T 'f',NT 11,NT 12,NT 5,NT 3],[T 'f',T 'o',NT 12,NT 5,NT 3],[T 'f',T 'o',T 'p',NT 5,NT 3],[T 'f',T 'o',T 'p',T 'b',NT 3],[T 'f',T 'o',T 'p',T 'b',NT 6,NT 7],[T 'f',T 'o',T 'p',T 'b',T 'c',NT 7],[T 'f',T 'o',T 'p',T 'b',T 'c',T 'd']]






--(NonLeaf 1 (NonLeaf 2 (NonLeaf 8 (Leaf 9 'f') (NonLeaf 10 (Leaf 11 'o') (Leaf 12 'p'))) (Leaf 5 'b')) (NonLeaf 3 (Leaf 6 'c') (Leaf 7 'd')))
--(NonLeaf 1 (NonLeaf 2 (NonLeaf 8 (Leaf 9 'f') (NonLeaf 10 (Leaf 11 'o') (Leaf 12 'p'))) (Leaf 5 'b')) (NonLeaf 3 (Leaf 6 'c') (Leaf 7 'd')))


--treeToRuleList (NonLeaf 1 (NonLeaf 2 (NonLeaf 8 (Leaf 9 'f')(NonLeaf 10 (Leaf 11 'o')(Leaf 12 'p'))) (Leaf 5 'b')) (NonLeaf 3(Leaf 6 'c')(Leaf 7 'd')))
--[NonterminalRule 1 (2,3),NonterminalRule 2 (8,5),NonterminalRule 8 (9,10),TerminalRule 9 'f',NonterminalRule 10 (11,12),TerminalRule 11 'o',TerminalRule 12 'p',TerminalRule 5 'b',NonterminalRule 3 (6,7),TerminalRule 6 'c',TerminalRule 7 'd']



