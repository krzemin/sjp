module ImpReductionSemantics where

-- http://www.cs.princeton.edu/courses/archive/spr96/cs441/notes/l9.html

import ImpAst
import ImpMemory
import Data.Maybe


data EvalCtxA = Hole
              | AddL EvalCtxA Aexp
              | AddR Numeral EvalCtxA
              deriving (Show, Eq)


plug :: EvalCtxA -> Aexp -> Aexp
plug Hole a = a
plug (AddL c a2) a1 = Add (plug c a1) a2 -- plug c (Add a1 a2)
plug (AddR a1 c) a2 = Add a1 (plug c a2)

data Dec = Val Numeral | Redex EvalCtxA Numeral Numeral
						-- | RedexV Ctx Identifier
--osobny typ Redex

dec :: Aexp -> Dec

dec (Num n) = Val n
dec (Add (Num n1) a2) = case dec a2 of
	Val n2 -> Redex Hole n1 n2
	Redex c n1' n2' -> Redex (AddR n1 c) n1' n2'
dec (Add a1 a2) = case dec a1 of
	Redex c n1' n2' -> Redex (AddL c a2) n1' n2'

plug' :: Dec -> Aexp

-- forall aexp . plug' (dec aexp) == aexp

contract :: Redex -> Aexp




plug :: Aexp -> EvalCtxA
plug (Num n) = Hole (Num n)
plug (Var x) = Hole (Var x)
plug (Add (Num n0) a1) = AddR (Num n0) (plug a1)
plug (Add a0 a1) = AddL (plug a0) a1



stepAexp :: (EvalCtxA, State) -> (EvalCtxA, State)

stepAexp (Hole (Var x), s) = (Hole (Num (fromJust $ lookupState x s)), s)
stepAexp (Hole (Add (Num n1) (Num n2)), s) = (Hole (Num (n1 + n2)), s)



main = do
    putStrLn $ show $ stepAexp (Hole (Var "x"), [("x", 10)])
    let a = Add (Add (Num 5) (Num 4)) (Num 2)
    let aPlugged = plug a
    putStrLn $ show $ aPlugged
