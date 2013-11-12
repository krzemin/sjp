module ImpReductionSemantics where

-- http://www.cs.princeton.edu/courses/archive/spr96/cs441/notes/l9.html

import ImpAst
import ImpMemory
import Data.Maybe


data EvalCtxA = Hole Aexp
              | AddL EvalCtxA Aexp
              | AddR Aexp EvalCtxA
              deriving (Show, Eq)

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
    --let aPlugged = plug a
    --putStrLn $ show $ aPlugged
