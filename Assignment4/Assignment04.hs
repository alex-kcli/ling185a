module Assignment04 where

import FSA
import SLG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fsa2 :: Automaton Int SegmentCV
fsa2 = (1, [3], [(1, C, 1),
                 (1, V, 1),
                 (1, C, 2),
                 (2, C, 2),
                 (2, V, 2),
                 (2, C, 3),
                 (3, C, 3),
                 (3, V, 3)])


--1: need one C, no need V
--2: balanced
--3: need one V, no need C
--4: need one C and one V

fsa3 :: Automaton Int SegmentCV
fsa3 = (1, [2], [(1, C, 2),
                 (1, V, 4),
                 (2, C, 1),
                 (2, V, 3),
                 (3, C, 4),
                 (3, V, 2),
                 (4, C, 3),
                 (4, V, 1)])

fsa4 :: Automaton Int SegmentCV
fsa4 = (1, [4], [(1, C, 1),
                 (1, V, 1),
                 (1, C, 2),
                 (2, C, 3),
                 (2, V, 3),
                 (3, C, 4),
                 (3, V, 4)])

fsa5 :: Automaton Int SegmentPKIU
fsa5 = (1, [1,2,3], [(1, P, 1),
                     (1, K, 1),
                     (1, I, 2),
                     (1, U, 3),
                     (1, WB, 1),
                     (2, P, 2),
                     (2, K, 2),
                     (2, I, 2),
                     (2, WB, 1),
                     (3, P, 3),
                     (3, K, 3),
                     (3, U, 3),
                     (3, WB, 1)])

fsa6 :: Automaton Int SegmentPKIU
fsa6 = (1, [1,2], [(1, P, 2),
                   (1, K, 1),
                   (1, I, 1),
                   (2, P, 2),
                   (2, K, 2),
                   (2, I, 2),
                   (2, U, 2)])

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (start, final, trans) =
    (ExtraState, map (\x -> StateForSymbol x) final, concat [map (\x -> (ExtraState, x, StateForSymbol x)) start, map (\(x,y) -> (StateForSymbol x, y, StateForSymbol y)) trans])
