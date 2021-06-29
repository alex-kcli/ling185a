module Assignment05 where

import FSA

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fwdProb :: (Ord st, Ord sy)
        => ProbFSA st sy
        -> [sy]
        -> st
        -> Double

fwdProb probFSA [str] st = sum (map (\previousSt -> ((trProb probFSA previousSt str st) * (initProb probFSA previousSt))) states)
    where
        states = allStatesPFSA probFSA

fwdProb probFSA strs st = sum (map (\previousSt -> ((trProb probFSA previousSt (last strs) st) * (fwdProb probFSA (init strs) previousSt))) states)
    where
        states = allStatesPFSA probFSA

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

unionFSAs :: (Ord sy)
          => EpsAutomaton Int sy
          -> EpsAutomaton Int sy
          -> EpsAutomaton Int sy
unionFSAs fstAuto sndAuto = EpsAutomaton(newStart, newEnd, newTrans)
    where
        newStart = 0
        newEnd = modFirstEnds ++ modSecondEnds
        newTrans = modFirstTrans ++ modSecondTrans ++ [(0, Nothing, modFirstStart),(0, Nothing, modSecondStart)]
        EpsAutomaton(modFirstStart, modFirstEnds, modFirstTrans) = ensureUnused [0] fstAuto
        EpsAutomaton(modSecondStart, modSecondEnds, modSecondTrans) = ensureUnused ([0] ++ (allStatesEFSA (EpsAutomaton(modFirstStart, modFirstEnds, modFirstTrans)))) sndAuto





concatFSAs :: (Ord sy)
            => EpsAutomaton Int sy
            -> EpsAutomaton Int sy
            -> EpsAutomaton Int sy
concatFSAs (EpsAutomaton(fstStart, fstEnd, fstTrans)) (EpsAutomaton(sndStart, sndEnd, sndTrans)) = (EpsAutomaton(newStart, newEnd, newTrans))
    where
        newStart = fstStart
        newEnd = modSecondEnd
        newTrans = fstTrans ++ modSecondTrans ++ (map (\firstend -> (firstend, Nothing, modSecondStart)) fstEnd)
        EpsAutomaton(modSecondStart, modSecondEnd, modSecondTrans) = ensureUnused (allStatesEFSA (EpsAutomaton(fstStart, fstEnd, fstTrans))) (EpsAutomaton(sndStart, sndEnd, sndTrans))





starFSA :: (Ord sy)
        => EpsAutomaton Int sy
        -> EpsAutomaton Int sy
starFSA auto = (EpsAutomaton(newStart, newEnd, newTrans))
    where
        newStart = 0
        newEnd = modEnd ++ [0]
        newTrans = modTrans ++ (map (\eend -> (eend, Nothing, modStart)) modEnd) ++ [(0, Nothing, modStart)]
        EpsAutomaton(modStart, modEnd, modTrans) = ensureUnused [0] auto

---------------------

reToFSA :: (Ord sy)
        => RegEx sy
        -> EpsAutomaton Int sy

reToFSA (Lit a) = EpsAutomaton(10, [11], [(10, Just a, 11)])

reToFSA (Alt r1 r2) = unionFSAs (reToFSA r1) (reToFSA r2)

reToFSA (Concat r1 r2) = concatFSAs (reToFSA r1) (reToFSA r2)

reToFSA (Star r) = starFSA (reToFSA r)

reToFSA Zero = EpsAutomaton(20, [], [])

reToFSA One = EpsAutomaton(30, [30], [])
