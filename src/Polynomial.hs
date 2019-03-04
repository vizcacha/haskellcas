module Polynomial where

type Term = (Integer, Char, Integer)
type Polynomial = [Term]

emptyTerm :: Term
emptyTerm = (0, 'x', 0)

degreeTerm :: Term -> Integer
degreeTerm (a, v, k) = k

leadingTerm :: Polynomial -> Term
leadingTerm p = leadingTerm' p emptyTerm

leadingTerm' :: Polynomial -> Term -> Term
leadingTerm' [] t = t
leadingTerm' (x:xs) t = if degreeTerm x > degreeTerm t then leadingTerm' xs x else leadingTerm' xs t
