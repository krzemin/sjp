module DenotImp where

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

liftSigma :: (State -> SigmaPeak) -> SigmaPeak -> SigmaPeak
liftSigma f (Aborted s) = Aborted s
liftSigma f (Normal s) = f s

evalCom :: Com -> State -> SigmaPeak
evalCom Skip s = Normal s
evalCom (Assign x a) s = Normal (insertState (x, evalAexp a s) s)
evalCom (Seq c0 c1) s = (liftSigma $ evalCom c1) (evalCom c0 s)
evalCom (If b c0 c1) s = if evalBexp b s then evalCom c0 s else evalCom c1 s
evalCom (While b c) s = (fix fun) (Normal s)
  where
    fun :: (SigmaPeak -> SigmaPeak) -> SigmaPeak -> SigmaPeak
    fun g (Normal s) = if evalBexp b s then  g (evalCom c s) else Normal s
    fun g (Aborted s) = Aborted s
    fix :: (a -> a) -> a
    fix f = let r = f r in r

evalCom Fail s = Aborted s


main :: IO ()
main = do
  print $ evalAexp (Add (Num 2) (Var "x")) [("x", 6)]
  print $ evalAexp (Add (Var "x") (Add (Num 2) (Num 1))) [("x", 6)]
  print $ evalBexp (Not (Leq (Num 4) (Var "x"))) [("x", 6)]
  let prog1 = (While (Leq (Var "x") (Num 5)) (Seq (Assign "y" (Add (Var "y") (Var "y"))) (Assign "x" (Add (Var "x") (Num 1)))))
  print $ evalCom prog1 [("x", 0), ("y", 2)]
  
