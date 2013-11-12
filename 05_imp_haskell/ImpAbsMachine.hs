module ImpAbsMachine where

import ImpAst
import ImpMemory
import Data.Maybe

data StackElem = Add1 Aexp
				| Add0 Numeral
				| Neg
				| And1 Bexp
				| Or1 Bexp
				| Leq1 Aexp
				| Leq0 Numeral
				| Asn Identifier
				| Cont Com
				| Sel Com Com
				deriving (Show, Eq)

data Current = A Aexp | B Bexp | C Com
				deriving (Show, Eq)

stepMachine :: (Current, [StackElem], State) -> (Current, [StackElem], State)

stepMachine (A (Var x), st, s) = (A (Num (fromJust $ lookupState x s)), st, s)

stepMachine (A (Num n1), (Add0 n0):st, s) = (A (Num (n0 + n1)), st, s)
stepMachine (A (Num n0), (Add1 a1):st, s) = (A a1, (Add0 n0):st, s)
stepMachine (A (Add a0 a1), st, s) = (A a0, (Add1 a1):st, s)


stepMachine (B (Not b), st, s) = (B b, Neg:st, s)
stepMachine (B T, Neg:st, s) = (B F, st, s)
stepMachine (B F, Neg:st, s) = (B T, st, s)

stepMachine (B (And b0 b1), st, s) = (B b0, (And1 b1):st, s)
stepMachine (B F, (And1 _):st, s) = (B F, st, s)
stepMachine (B T, (And1 b1):st, s) = (B b1, st, s)

stepMachine (B (Or b0 b1), st, s) = (B b0, (Or1 b1):st, s)
stepMachine (B T, (Or1 _):st, s) = (B T, st, s)
stepMachine (B F, (Or1 b1):st, s) = (B b1, st, s)

stepMachine (B (Leq a0 a1), st, s) = (A a0, (Leq1 a1):st, s)
stepMachine (A (Num n0), (Leq1 a1):st, s) = (A a1, (Leq0 n0):st, s)
stepMachine (A (Num n1), (Leq0 n0):st, s)
	| n0 <= n1 = (B T, st, s)
	| otherwise = (B F, st, s)


stepMachine (C (Assign x a), st, s) = (A a, (Asn x):st, s)
stepMachine (A (Num n), (Asn x):st, s) = (C Skip, st, insertState (x, n) s) 

stepMachine (C (Seq c1 c2), st, s) = (C c1, (Cont c2):st, s)
stepMachine (C Skip, (Cont c2):st, s) = (C c2, st, s)

stepMachine (C (If b c1 c2), st, s) = (B b, (Sel c1 c2):st, s)
stepMachine (B T, (Sel c1 _):st, s) = (C c1, st, s)
stepMachine (B F, (Sel _ c2):st, s) = (C c2, st, s)

stepMachine (C (While b c), st, s) = (C (If b (Seq c (While b c)) Skip), st, s)

transitiveClosure :: (Current, [StackElem], State) -> State

transitiveClosure (C Skip, [], s) = s
transitiveClosure (c, st, s) = transitiveClosure (c', st', s')
	where (c', st', s') = stepMachine (c, st, s)

absMachine :: (Com, State) -> State
absMachine (c, s) = transitiveClosure (C c, [], s)


main = do
	putStrLn $ show $ stepMachine (A $ Add (Num 2) (Var "x"), [], [("x", 10)])
	putStrLn $ show $ stepMachine (A $ (Num 2), [Asn "x"], [("x", 10)])
	putStrLn $ show $ absMachine (While (Leq (Var "x") (Num 10)) (Assign "x" (Add (Var "x") (Num 1))), [("x", 0)])





