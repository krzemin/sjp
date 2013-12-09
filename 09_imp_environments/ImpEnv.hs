module ImpEnv where

import Data.Maybe

type Identifier = String
type ProcName = Identifier

data Aexp = Num Int
          | Var Identifier
          | Add Aexp Aexp
          deriving (Show, Eq)

data Bexp = T
          | F
          | Not Bexp
          | Leq Aexp Aexp
          deriving (Show, Eq)

data Com = Skip
         | Assign Identifier Aexp
         | Seq Com Com
         | If Bexp Com Com
         | While Bexp Com
         | Block [DeclV] [DeclP] Com
         | Call ProcName Aexp
         deriving (Show, Eq)

data DeclV = Dim Identifier Aexp deriving (Show, Eq)
data DeclP = Proc ProcName Identifier Com deriving (Show, Eq)

type State = [(Identifier, Int)]

lookupState :: Identifier -> State -> Maybe Int
lookupState = lookup

insertState :: (Identifier, Int) -> State -> State
insertState (x, n) lst = (x, n) : lst


evalAexp :: Aexp -> State -> Int
evalAexp (Num n) _ = n
evalAexp (Var x) s = fromJust $ lookupState x s
evalAexp (Add a0 a1) s = evalAexp a0 s + evalAexp a1 s

evalBexp :: Bexp -> State -> Bool
evalBexp T _ = True
evalBexp F _ = False
evalBexp (Not b) s = not $ evalBexp b s
evalBexp (Leq a0 a1) s = evalAexp a0 s <= evalAexp a1 s




prog1 :: Com
prog1 = Block
					[Dim "x" (Num 5)]
					[Proc "addx" "a" (Assign "x" (Add (Var "x") (Var "a")))]
					(Call "addx" (Num 100))
