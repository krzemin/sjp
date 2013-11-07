-- Piotr Krzemiński, Semantyka Języków Programowania, 2013/14
-- Lista 5, zad. 1

module ImpSos where

import ImpAst
import ImpMemory
import Data.Maybe


stepAexp :: (Aexp, State) -> (Aexp, State)

stepAexp (Var x, s) = (Num $ fromJust $ lookupState x s, s)

stepAexp (Add (Num n1) (Num n2), s) = (Num (n1 + n2), s)
stepAexp (Add (Num n1) a2, s) = (Add (Num n1) a2', s)
	where (a2', _) = stepAexp (a2, s)
stepAexp (Add a1 a2, s) = (Add a1' a2, s)
	where (a1', _) = stepAexp (a1, s)


stepBexp :: (Bexp, State) -> (Bexp, State)

stepBexp (Not F, s) = (T, s)
stepBexp (Not T, s) = (F, s)
stepBexp (Not b, s) = (b', s)
	where (b', _) = stepBexp (b, s)

stepBexp (And F _, s) = (F, s)
stepBexp (And T F, s) = (F, s)
stepBexp (And T T, s) = (T, s)
stepBexp (And T b2, s) = (And T b2', s)
	where (b2', _) = stepBexp (b2, s)
stepBexp (And b1 b2, s) = (And b1' b2, s)
	where (b1', _) = stepBexp (b1, s)

stepBexp (Or T _, s) = (T, s)
stepBexp (Or F T, s) = (T, s)
stepBexp (Or F F, s) = (F, s)
stepBexp (Or F b2, s) = (Or F b2', s)
	where (b2', _) = stepBexp (b2, s)
stepBexp (Or b1 b2, s) = (Or b1' b2, s)
	where (b1', _) = stepBexp (b1, s)

stepBexp (Leq (Num n1) (Num n2), s)
	| n1 <= n2 = (T, s)
	| otherwise = (F, s)
stepBexp (Leq (Num n1) a2, s) = (Leq (Num n1) a2', s)
	where (a2', _) = stepAexp (a2, s)
stepBexp (Leq a1 a2, s) = (Leq a1' a2, s)
	where (a1', _) = stepAexp (a1, s)


stepCom :: (Com, State) -> (Com, State)

stepCom (Assign x (Num n), s) = (Skip, insertState (x, n) s)
stepCom (Assign x a, s) = (Assign x a', s)
	where (a', _) = stepAexp (a, s)

stepCom (Seq Skip c2, s) = (c2, s)
stepCom (Seq c1 c2, s) = (Seq c1' c2, s')
	where (c1', s') = stepCom (c1, s)

stepCom (If T c1 _, s) = (c1, s)
stepCom (If F _ c2, s) = (c2, s)
stepCom (If b c1 c2, s) = (If b' c1 c2, s)
	where (b', _) = stepBexp (b, s)

stepCom (While b c, s) = (If b (Seq c (While b c)) Skip, s)


transitiveClosure :: (Com, State) -> State

transitiveClosure (Skip, s) = s
transitiveClosure (c, s) = transitiveClosure (c', s')
	where (c', s') = stepCom (c, s)


sos :: (Com, State) -> State
sos = transitiveClosure

main = do
	putStrLn $ show $ stepAexp (Add (Var "x") (Num 4), [("x", 100)])
	putStrLn $ show $ stepBexp (Or F (And T T), [])
	putStrLn $ show $ stepBexp (Leq (Num 3) (Add (Num 1) (Num 10)), [])
	putStrLn $ show $ stepCom (Assign "x" (Num 4), [])
	putStrLn $ show $ stepCom (If T (Assign "x" (Num 4)) Skip, [])
	putStrLn $ show $ stepCom (While (Or F T) (Assign "x" (Num 2)), [])
	putStrLn $ show $ sos (While (Leq (Var "x") (Num 10)) (Assign "x" (Add (Var "x") (Num 1))), [("x", 0)])

