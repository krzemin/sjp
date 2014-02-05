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
          | VSum (Expr, Expr)
          | VInl Expr
          | VInr Expr
          | VExpr Expr

instance Show Val where
  show (VN n) = show n
  show (VB b) = show b
  show (VFun _) = "@function"
  show (VSum p) = show p
  show (VInl e) = "inl@" ++ show e
  show (VInr e) = "inr@" ++ show e
  show (VExpr e) = "code@" ++ show e

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

typedS :: ((Expr, Expr) -> Val') -> Val -> Val'
typedS f (VSum x) = f x
typedS _ _ = TypeErr "expected sum"

typedLR :: (Expr -> Val') -> Val -> Val'
typedLR f (VInl x) = f x
typedLR f (VInr x) = f x
typedLR _ _ = TypeErr "expected inl or inr"

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
  where k' f = evalExpr e2 env (`f` k)
evalExpr (Pair e1 e2) env k = k (VSum (e1, e2))
evalExpr (Proj1 e) env k = evalExpr e env (typedS k')
  where k' (e1, _) = evalExpr e1 env k
evalExpr (Proj2 e) env k = evalExpr e env (typedS k')
  where k' (_, e2) = evalExpr e2 env k
evalExpr (Inl e) env k = k (VInl e)
evalExpr (Inr e) env k = k (VInr e)
evalExpr (Case e x1 e1 x2 e2) env k = evalExpr e env (typedLR k')
  where k' (VInl e1') = evalExpr e1 (insert x1 (VExpr e1') env) k
        k' (VInr e2') = evalExpr e2 (insert x2 (VExpr e2') env) k
evalExpr (Let x e0 e) env k = evalExpr (App (Lam x e) e0) env k
evalExpr (LetRec x e0 e) env k = evalExpr (Let x (Rec (Lam x e)) e) env k
--evalExpr (LetRec x y e0 e) env k = evalExpr e (insert x (VFun f) env) k
--  where f = fix (\g v k' -> evalExpr e0 (insert y v (insert x (VFun g) env)) k')

eval :: Expr -> Val'
eval expr = evalExpr expr empty OK

main :: IO ()
main = do
  let fib k = LetRec "fib" (Lam "n" (If (Lt (Var "n") (N 2))
        (Var "n")
        (Add
          (App (Var "fib") (Sub (Var "n") (N 1)))
          (App (Var "fib") (Sub (Var "n") (N 2)))
        ) ) ) (App (Var "fib") (N k))
  print $ map (eval . fib) [0..15]



