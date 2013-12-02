module DenotImpOnlyFailuresCont where

import           Data.Maybe

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

data SigmaPeak = Normal State | Aborted State deriving (Show, Eq)

evalCom :: Com -> (State -> SigmaPeak) -> State -> SigmaPeak
evalCom Skip k s = k s
evalCom (Assign x a) k s = k (insertState (x, evalAexp a s) s)
evalCom (Seq c0 c1) k s = evalCom c0 (evalCom c1 k) s
evalCom (If b c0 c1) k s = if evalBexp b s then evalCom c0 k s else evalCom c1 k s
evalCom (While b c) k s = fix fun s
  where
    fun :: (State -> SigmaPeak) -> State -> SigmaPeak
    fun w si = if evalBexp b si then evalCom c w si else k si
    fix :: (a -> a) -> a
    fix f = let r = f r in r

evalCom Fail _ s = Aborted s


main :: IO ()
main = do
  print $ evalAexp (Add (Num 2) (Var "x")) [("x", 6)]
  print $ evalAexp (Add (Var "x") (Add (Num 2) (Num 1))) [("x", 6)]
  print $ evalBexp (Not (Leq (Num 4) (Var "x"))) [("x", 6)]
  let prog1 = While (Leq (Var "x") (Num 5)) (Seq (Assign "y" (Add (Var "y") (Var "y"))) (Assign "x" (Add (Var "x") (Num 1))))
  print $ evalCom prog1 Normal [("x", 0), ("y", 2)]
  let prog2 = While T Fail
  print $ evalCom prog2 Normal []
  
