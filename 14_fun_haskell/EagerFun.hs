module EagerFun where

import Prelude hiding (lookup)
import Data.Map
import Data.Function

type Ide = String

data Expr = N Int
          | B Bool
          | Minus Expr
          | Not Expr
          | Add Expr Expr
          | And Expr Expr
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
          | LetRec Ide Ide Expr Expr

type Cont = Val -> Val'

data Val =  VN Int
          | VB Bool
          | VFun (Val -> Cont -> Val')
          | VSum (Val, Val)
          | VInl Val
          | VInr Val

data Val' = OK Val | Err | TypeErr String

type Env = Map Ide Val

typedN :: (Int -> Val') -> Val -> Val'
typedN f (VN x) = f x
typedN _ _ = TypeErr "expected int"

typedB :: (Bool -> Val') -> Val -> Val'
typedB f (VB x) = f x
typedB _ _ = TypeErr "expected bool"

typedF :: ((Val -> Cont -> Val') -> Val') -> Val -> Val'
typedF f (VFun x) = f x
typedF _ _ = TypeErr "expected fun"

typedS :: ((Val, Val) -> Val') -> Val -> Val'
typedS f (VSum x) = f x
typedS _ _ = TypeErr "expected sum"

typedLR :: (Val -> Val') -> Val -> Val'
typedLR f (VInl x) = f x
typedLR f (VInr x) = f x
typedLR _ _ = TypeErr "expected inl or inr"


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
evalExpr (And e1 e2) env k = evalExpr e1 env (typedB k')
  where k' b1 = evalExpr e2 env (typedB (k'' b1))
        k'' b1 b2 = k (VB (b1 && b2))
evalExpr (If e e1 e2) env k = evalExpr e env (typedB k')
  where k' True = evalExpr e1 env k
        k' False = evalExpr e2 env k
evalExpr (Var x) env k = case lookup x env of
  Nothing -> Err
  Just v -> k v
evalExpr (Lam x e) env k = k (VFun f)
  where f v = evalExpr e (insert x v env) 
evalExpr (App e1 e2) env k = evalExpr e1 env (typedF k')
  where k' f = evalExpr e2 env (`f` k)
evalExpr (Pair e1 e2) env k = evalExpr e1 env k'
  where k' v1 = evalExpr e2 env (k'' v1)
        k'' v1 v2 = k (VSum (v1, v2))
evalExpr (Proj1 e) env k = evalExpr e env (typedS k')
  where k' (v1, _) = k v1
evalExpr (Proj2 e) env k = evalExpr e env (typedS k')
  where k' (_, v2) = k v2
evalExpr (Inl e) env k = evalExpr e env k'
  where k' v = k (VInl v)
evalExpr (Inr e) env k = evalExpr e env k'
  where k' v = k (VInr v)
evalExpr (Case e x1 e1 x2 e2) env k = evalExpr e env (typedLR k')
  where k' (VInl v1) = evalExpr e1 (insert x1 v1 env) k
        k' (VInr v2) = evalExpr e2 (insert x2 v2 env) k
evalExpr (Let x e0 e) env k = evalExpr (App (Lam x e) e0) env k
evalExpr (LetRec x y e0 e) env k = evalExpr e (insert x (VFun f) env) k
  where f = fix (\g v k' -> evalExpr e0 (insert y v (insert x (VFun g) env)) k')

eval :: Expr -> Val'
eval expr = evalExpr expr empty OK

