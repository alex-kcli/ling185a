module Assignment01 where

data WFF = T | F | Neg WFF
         | Conj WFF WFF | Disj WFF WFF deriving Show

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

denotation :: WFF -> Bool
denotation d = case d of
    T -> True
    F -> False
    Neg x -> case x of {T -> False; F-> True}
    Conj x y -> denotation x && denotation y
    Disj x y -> denotation x || denotation y
