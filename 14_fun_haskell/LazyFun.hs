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
          | Pair Expr Expr
          | Proj1 Expr
          | Proj2 Expr
          | Inl Expr
          | Inr Expr
          | Case Expr Ide Expr Ide Expr
          | Let Ide Expr Expr
          | LetRec Ide Expr Expr
          | Rec Expr
          deriving (Show)

type Cont = Val -> Val'

data Val =  VN Int
          | VB Bool
          | VFun (Val' -> Cont -> Val')
          | VSum (Cont -> Val', Cont -> Val')
          | VInl (Cont -> Val')
          | VInr (Cont -> Val')

instance Show Val where
  show (VN n) = show n
  show (VB b) = show b
  show (VFun _) = "@function"
  show (VSum _) = "@sum"
  show (VInl _) = "@inl"
  show (VInr _) = "@inr"

data Val' = OK Val | Err | TypeErr String deriving (Show)

type Env = Map Ide Val'

typedN :: (Int -> Val') -> Val -> Val'
typedN f (VN x) = f x
typedN _ _ = TypeErr "expected int"

typedB :: (Bool -> Val') -> Val -> Val'
typedB f (VB x) = f x
typedB _ _ = TypeErr "expected bool"

typedF :: ((Val' -> Cont -> Val') -> Val') -> Val -> Val'
typedF f (VFun x) = f x
typedF _ _ = TypeErr "expected fun"

typedS :: ((Cont -> Val', Cont -> Val') -> Val') -> Val -> Val'
typedS f (VSum x) = f x
typedS _ _ = TypeErr "expected sum"

typedLR :: ((Cont -> Val') -> Val') -> ((Cont -> Val') -> Val') -> Val -> Val'
typedLR f _ (VInl x) = f x
typedLR _ g (VInr x) = g x
typedLR _ _ _ = TypeErr "expected inl or inr"

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
  where f v = evalExpr e (insert x v env)
evalExpr (App e1 e2) env k = evalExpr e1 env (typedF k')
  where k' f = evalExpr e2 env (\v -> f (OK v) k)
evalExpr (Pair e1 e2) env k = k (VSum (evalExpr e1 env, evalExpr e2 env))
evalExpr (Proj1 e) env k = evalExpr e env (typedS k')
  where k' (f1, _) = f1 k
evalExpr (Proj2 e) env k = evalExpr e env (typedS k')
  where k' (_, f2) = f2 k
evalExpr (Inl e) env k = k (VInl (evalExpr e env))
evalExpr (Inr e) env k = k (VInr (evalExpr e env))
evalExpr (Case e x1 e1 x2 e2) env k = evalExpr e env (typedLR kl kr)
  where kl e1' = evalExpr e1 (insert x1 (e1' OK) env) k
        kr e2' = evalExpr e2 (insert x2 (e2' OK) env) k
evalExpr (Let x e0 e) env k = evalExpr (App (Lam x e) e0) env k
evalExpr (LetRec x e0 e) env k = evalExpr (Let x (Rec (Lam x e0)) e) env k
evalExpr (Rec e) env k = evalExpr e env (typedF k')
  where k' f = star k $ fix (`f` OK)

eval :: Expr -> Val'
eval expr = evalExpr expr empty OK

main :: IO ()
main = do
  let expr1 = App (Lam "x" (Add (Var "x") (N 100))) (Add (N 10) (N 20))
  print $ eval expr1

  let expr2 = Pair (N 1) (Var "notexists")
  print $ eval expr2
  print $ eval (Proj1 expr2)
  print $ eval (Proj2 expr2)

  let expr3 = Pair (N 1) (Pair (N 2) (Var "notexists"))
  print $ eval expr3
  print $ eval (Proj1 expr3)
  print $ eval ((Proj1 . Proj2) expr3)
  print $ eval ((Proj2 . Proj2) expr3)

  let expr4 = Rec (Lam "ones" (Pair (N 1) (Var "ones")))
  print $ eval expr4
  print $ eval (Proj1 expr4)
  print $ eval (Proj2 expr4)
  print $ eval ((Proj1 . Proj2) expr4)

  let expr5 = App (Rec (Lam "inf" (Lam "init" (Pair (Var "init") (App (Var "inf") (Add (Var "init") (N 1))))))) (N 0)
  print $ eval expr5
  print $ eval (Proj1 expr5)
  print $ eval (Proj2 expr5)
  print $ eval ((Proj1 . Proj2) expr5)

  let fib k = LetRec "fib" (Lam "n" (If (Lt (Var "n") (N 2))
        (Var "n")
        (Add
          (App (Var "fib") (Sub (Var "n") (N 1)))
          (App (Var "fib") (Sub (Var "n") (N 2)))
        ) ) ) (App (Var "fib") (N k))
  print $ map (eval . fib) [0..15]
  


