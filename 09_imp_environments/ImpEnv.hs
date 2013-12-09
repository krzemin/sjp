module ImpEnv where

import Data.Maybe

type IdeV = String
type IdeP = String

data Aexp = Num Int
          | Var IdeV
          | Add Aexp Aexp
          deriving (Show, Eq)

data Bexp = T
          | F
          | Not Bexp
          | Leq Aexp Aexp
          deriving (Show, Eq)

data Com = Skip
         | Assign IdeV Aexp
         | Seq Com Com
         | If Bexp Com Com
         | While Bexp Com
         | Block [DeclV] [DeclP] Com
         | Call IdeP Aexp
         deriving (Show, Eq)

data DeclV = Dim IdeV Aexp deriving (Show, Eq)
data DeclP = Proc IdeP IdeV Com deriving (Show, Eq)

type Loc = Int
data Store = Sto [(Loc, Int)] Loc -- memory and next free location
type EnvV = [(IdeV, Loc)]
type EnvP = [(IdeP, Loc)]

-- memory management
new :: Loc -> Loc
new n = n + 1 

lookupEnvV :: EnvV -> IdeV -> Maybe Loc
lookupEnvV envV v = lookup v envV

lookupV :: EnvV -> Store -> IdeV -> Maybe Int
lookupV envV (Sto sto _) v = do
  loc <- lookupEnvV envV v
  lookup loc sto

updateSto :: Loc -> Int -> Store -> Store
updateSto loc val (Sto sto next)  = Sto sto' next
  where
    sto' = update sto
    update ((x, v) : rest)
      | x == loc = (loc, val) : rest
      | otherwise = (x, v) : update rest

-- denotational semantics

evalAexp :: Aexp -> EnvV -> Store -> Int
evalAexp (Num n) _ _ = n
evalAexp (Var x) envV sto = fromJust $ lookupV envV sto x
evalAexp (Add a0 a1) envV sto = evalAexp a0 envV sto + evalAexp a1 envV sto

evalBexp :: Bexp -> EnvV -> Store -> Bool
evalBexp T _ _ = True
evalBexp F _ _ = False
evalBexp (Not b) envV sto = not $ evalBexp b envV sto
evalBexp (Leq a0 a1) envV sto = evalAexp a0 envV sto <= evalAexp a1 envV sto

evalCom :: Com -> EnvV -> Store -> Store
evalCom Skip _ sto = sto
evalCom (Assign x a) envV sto = updateSto loc val sto
  where
    loc = fromJust $ lookupV envV sto x
    val = evalAexp a envV sto
evalCom (Seq c0 c1) envV sto = evalCom c1 envV $ evalCom c0 envV sto
evalCom (If b c0 c1) envV sto =
  if evalBexp b envV sto
    then evalCom c0 envV sto
    else evalCom c1 envV sto
evalCom (While b c) envV sto = fix fun sto
  where
    fun :: (Store -> Store) -> Store -> Store
    fun g st = if evalBexp b envV st then g (evalCom c envV st) else st 
    fix :: (a -> a) -> a
    fix f = let r = f r in r



prog1 :: Com
prog1 = Block
					[Dim "x" (Num 5)]
					[Proc "addx" "a" (Assign "x" (Add (Var "x") (Var "a")))]
					(Call "addx" (Num 100))
