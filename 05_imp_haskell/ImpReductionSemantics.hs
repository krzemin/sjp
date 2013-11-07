module ImpReductionSemantics where

import ImpAst
import ImpMemory
import Data.Maybe


data EvalCtxA = Hole Aexp
			| AddL EvalCtxA Aexp
			| AddR Numeral EvalCtxA
			deriving (Show, Eq)


stepAexp :: (EvalCtxA, State) -> (EvalCtxA, State)

stepAexp (Hole (Var x), s) = (Hole (Num (fromJust $ lookupState x s)), s)
stepAexp (Hole (Add (Num n1) (Num n2)), s) = (Hole (Num (n1 + n2)), s)



main = do
	putStrLn $ show $ stepAexp (Hole (Var "x"), [("x", 10)])
