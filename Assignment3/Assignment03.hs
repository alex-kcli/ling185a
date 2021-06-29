module Assignment03 where

--import RegEx
--import SLG

import Data.List (nub)

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














-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

bigrams :: [a] -> [(a, a)]
bigrams [] = []
bigrams [_] = []
bigrams [x, y] = [(x, y)]
bigrams (x:xs) = [(x, (xs !! 0))] ++ bigrams xs

pretty :: (Eq a) => [(a, a)] -> [a]
pretty [] = []
pretty [(x, y)] = [x] ++ [y]
pretty ((x, y):zs) = case (isChained ((x, y):zs)) of True -> [x] ++ pretty zs
                                                     False -> []

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows (start, final, trans) var = 
    map snd (filter (\x -> (fst x == var)) trans)

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes (start, final, trans) var = 
    map fst (filter (\x -> (snd x == var)) trans)

-------------------------------------------------------------------------------

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the"],["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very"],["very","very","very"],["very","very","fat"],
--    ["very","fat","cat"],["very","very"],["very","fat"]]

forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward m_SLG num var = go m_SLG num [[var]]
  where
    go m_SLG num [[]] = [[]]
    go m_SLG 0 listVar = [[var]]
    go m_SLG num listVar = (go m_SLG (num-1) result) ++ result
      where
        result = returnResultForward m_SLG listVar

-- forward helper function
returnResultForward :: (Eq sy) => SLG sy -> [[sy]] -> [[sy]]
returnResultForward mslg (x:[]) = map (\y -> concat [x, [y]]) (follows mslg (last x))
returnResultForward mslg (x:xs) = 
    map (\y -> concat [x, [y]]) (follows mslg (last x)) ++ returnResultForward mslg xs

-------------------------------------------------------------------------------

-- MORE EXAMPLE USAGE:
-- backward g2 1 "cat"
-- => [["cat"],["the","cat"],["fat","cat"]]
-- backward g2 2 "very"
-- => [["very"],["the","very","very"],["very","very","very"],["the","very"],
--    ["very","very"]]

backward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
backward m_SLG num var = go m_SLG num [[var]]
  where
    go m_SLG num [[]] = [[]]
    go m_SLG 0 listVar = [[var]]
    go m_SLG num listVar = (go m_SLG (num-1) result) ++ result
      where
        result = returnResultBackward m_SLG listVar

-- backward helper function
returnResultBackward :: (Eq sy) => SLG sy -> [[sy]] -> [[sy]]
returnResultBackward mslg (x:[]) = map (\y -> concat [[y], x]) (precedes mslg (head x))
returnResultBackward mslg (x:xs) = 
    map (\y -> concat [[y], x]) (precedes mslg (head x)) ++ returnResultBackward mslg xs

-------------------------------------------------------------------------------

generates :: (Eq sy) => SLG sy -> Int -> [[sy]]
generates ([start], [final], trans) num = 
    filter (\x -> (last x == final)) (forward ([start], [final], trans) num start)
generates (startList, finalList, trans) num = 
    filter (\x -> (last x) `elem` finalList) (concat (map (\z -> forward (startList, finalList, trans) num z) startList))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

occurrences :: Int -> (RegEx a) -> (RegEx a)
occurrences 0 reg = One
occurrences 1 reg = reg
occurrences num reg = Concat reg (occurrences (num-1) reg)

optional :: (RegEx a) -> (RegEx a)
optional reg = Alt (reg) One
