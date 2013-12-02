module DenotImp where

import Data.Maybe

type Identifier = String
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
         | Fail
         | Read Identifier
         | Write Aexp
         deriving (Show, Eq)

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


main :: IO ()
main = do
  print $ evalAexp (Add (Num 2) (Var "x")) [("x", 6)]
  print $ evalAexp (Add (Var "x") (Add (Num 2) (Num 1))) [("x", 6)]
  print $ evalBexp (Not (Leq (Num 4) (Var "x"))) [("x", 6)]
