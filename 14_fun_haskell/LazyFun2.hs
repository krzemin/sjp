module LazyFun where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.Function

type Ide = String

data Expr = N Int
          | B Bool
          | Minus Expr
          | Not Expr
          | Add Expr Expr
          | Sub Expr Expr
          | And Expr Expr
          | Lt Expr Expr
          | If Expr Expr Expr
          | Var Ide
          | Lam Ide Expr
          | App Expr Expr
          | Let Ide Expr Expr
          | LetRec Ide Expr Expr
          | Rec Expr
          deriving (Show)

type Cont = Val -> Val'

data Val =  VN Int
          | VB Bool
          | VFun ((Cont -> Val') -> Cont -> Val')

instance Show Val where
  show (VN n) = show n
  show (VB b) = show b
  show (VFun _) = "@function"

data Val' = OK Val | Err | TypeErr String deriving (Show)

type Env = Map Ide Val'

typedN :: (Int -> Val') -> Val -> Val'
typedN f (VN x) = f x
typedN _ _ = TypeErr "expected int"

typedB :: (Bool -> Val') -> Val -> Val'
typedB f (VB x) = f x
typedB _ _ = TypeErr "expected bool"

typedF :: (((Cont -> Val') -> Cont -> Val') -> Val') -> Val -> Val'
typedF f (VFun x) = f x
typedF _ _ = TypeErr "expected fun"

star :: (Val -> Val') -> Val' -> Val'
star f (OK x) = f x
star _ Err = Err
star _ (TypeErr x) = TypeErr x

evalExpr :: Expr -> Env -> Cont -> Val'
evalExpr (N n) _ k = k (VN n)
evalExpr (B b) _ k = k (VB b)
evalExpr (Minus e) env k = evalExpr e env (typedN k')
  where k' n = k (VN (-n))
evalExpr (Not e) env k = evalExpr e env (typedB k')
  where k' b = k (VB (not b))
evalExpr (Add e1 e2) env k = evalExpr e1 env (typedN k')
  where k' n1 = evalExpr e2 env (typedN (k'' n1))
        k'' n1 n2 = k (VN (n1 + n2))
evalExpr (Sub e1 e2) env k = evalExpr e1 env (typedN k')
  where k' n1 = evalExpr e2 env (typedN (k'' n1))
        k'' n1 n2 = k (VN (n1 - n2))
evalExpr (And e1 e2) env k = evalExpr e1 env (typedB k')
  where k' b1 = evalExpr e2 env (typedB (k'' b1))
        k'' b1 b2 = k (VB (b1 && b2))
evalExpr (Lt e1 e2) env k = evalExpr e1 env (typedN k')
  where k' n1 = evalExpr e2 env (typedN (k'' n1))
        k'' n1 n2 = k (VB (n1 < n2))
evalExpr (If e e1 e2) env k = evalExpr e env (typedB k')
  where k' True = evalExpr e1 env k
        k' False = evalExpr e2 env k
evalExpr (Var x) env k = case lookup x env of
  Nothing -> Err
  Just v -> star k v
evalExpr (Lam x e) env k = k (VFun f)
  where f kv = evalExpr e (insert x (kv OK) env)
evalExpr (App e1 e2) env k = evalExpr e1 env (typedF k')
  where k' f = f (evalExpr e2 env) k
evalExpr (Let x e0 e) env k = evalExpr (App (Lam x e) e0) env k
evalExpr (LetRec x e0 e) env k = evalExpr (Let x (Rec (Lam x e0)) e) env k
evalExpr (Rec e) env k = evalExpr e env (typedF k')
  where k' f = star k $ fix (\v -> f (const v) k)

eval :: Expr -> Val'
eval expr = evalExpr expr empty OK

main :: IO ()
main = do
  let expr1 = App (Lam "x" (Add (Var "x") (N 100))) (Add (N 10) (N 20))
  print $ eval expr1

  let fib k = LetRec "fib" (Lam "n" (If (Lt (Var "n") (N 2))
        (Var "n")
        (Add
          (App (Var "fib") (Sub (Var "n") (N 1)))
          (App (Var "fib") (Sub (Var "n") (N 2)))
        ) ) ) (App (Var "fib") (N k))
  print $ map (eval . fib) [0..15]

  let expr6 = LetRec "f"
        (Lam "x" (App (Var "f") (Var "x")))
        (App (Lam "a" (N 42)) (App (Var "f") (N 69)))
  print $ eval expr6
  


