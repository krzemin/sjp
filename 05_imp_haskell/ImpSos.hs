-- Piotr Krzemiński, Semantyka Języków Programowania, 2013/14
-- Lista 5, zad. 1

module ImpSos where

import Data.Maybe

type Numeral = Int
type Identifier = String
data Aexp = Num Numeral
          | Var Identifier
          | Add Aexp Aexp
          deriving (Show, Eq)
data Bexp = T
          | F
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Leq Aexp Aexp
          deriving (Show, Eq)
data Com = Skip
          | Assign Identifier Aexp
          | Seq Com Com
          | If Bexp Com Com
          | While Bexp Com
          deriving (Show, Eq)

type State = [(Identifier, Numeral)]

lookupState :: Identifier -> State -> Maybe Numeral
lookupState = lookup

insertState :: (Identifier, Numeral) -> State -> State
insertState (x, n) lst = (x, n) : lst

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







main = do
	putStrLn $ show $ stepAexp (Add (Var "x") (Num 4), [("x", 100)])
	putStrLn $ show $ stepBexp (Or F (And T T), [])
	putStrLn $ show $ stepBexp (Leq (Num 3) (Add (Num 1) (Num 10)), [])



