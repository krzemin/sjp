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
data Store = Sto [(Loc, Int)] Loc deriving (Show, Eq) -- memory and next free location
type EnvV = [(IdeV, Loc)]
type EnvP = [(IdeP, Store -> Store)]

-- memory management
new :: Loc -> Loc
new n = n + 1 

lookupEnvV :: EnvV -> IdeV -> Maybe Loc
lookupEnvV envV v = lookup v envV

updateEnvV :: IdeV -> Loc -> EnvV -> EnvV
updateEnvV x loc = update
  where
    update [] = [(x, loc)]
    update ((y, l) : rest)
      | x == y = (x, loc) : rest
      | otherwise = (y, l) : update rest

lookupEnvP :: EnvP -> IdeP -> Maybe (Store -> Store)
lookupEnvP envP p = lookup p envP

updateEnvP :: IdeP -> (Store -> Store) -> EnvP -> EnvP
updateEnvP p proc = update
  where
    update [] = [(p, proc)]
    update ((q, proc') : rest)
      | p == q = (p, proc) : rest
      | otherwise = (q, proc') : update rest


lookupV :: EnvV -> [(Loc, Int)] -> IdeV -> Maybe Int
lookupV envV sto v = do
  loc <- lookupEnvV envV v
  lookup loc sto

updateSto :: Loc -> Int -> [(Loc, Int)] -> [(Loc, Int)]
updateSto loc val = update
  where
    update [] = [(loc, val)]
    update ((x, v) : rest)
      | x == loc = (loc, val) : rest
      | otherwise = (x, v) : update rest

-- denotational semantics

evalAexp :: Aexp -> EnvV -> Store -> Int
evalAexp (Num n) _ _ = n
evalAexp (Var x) envV (Sto sto _) = fromJust $ lookupV envV sto x
evalAexp (Add a0 a1) envV sto = evalAexp a0 envV sto + evalAexp a1 envV sto

evalBexp :: Bexp -> EnvV -> Store -> Bool
evalBexp T _ _ = True
evalBexp F _ _ = False
evalBexp (Not b) envV sto = not $ evalBexp b envV sto
evalBexp (Leq a0 a1) envV sto = evalAexp a0 envV sto <= evalAexp a1 envV sto

evalCom :: Com -> EnvV -> EnvP -> Store -> Store
evalCom Skip _ _ sto = sto
evalCom (Assign x a) envV _ s@(Sto sto next) = Sto sto' next
  where
    sto' = updateSto loc val sto
    loc = fromJust $ lookupEnvV envV x
    val = evalAexp a envV s
evalCom (Seq c0 c1) envV envP sto = evalCom c1 envV envP $ evalCom c0 envV envP sto
evalCom (If b c0 c1) envV envP sto =
  if evalBexp b envV sto
    then evalCom c0 envV envP sto
    else evalCom c1 envV envP sto
evalCom (While b c) envV envP sto = fix fun sto
  where
    fun :: (Store -> Store) -> Store -> Store
    fun g st = if evalBexp b envV st then g (evalCom c envV envP st) else st 
    fix :: (a -> a) -> a
    fix f = let r = f r in r
evalCom (Block declsV declsP c) envV envP sto = evalCom c envV' envP' sto'
  where
    (envV', sto') = evalDeclsV declsV (envV, sto)
    envP' = evalDeclsP declsP envV' envP
evalCom (Call p _) _ envP sto = (fromJust $ lookupEnvP envP p) sto

evalDeclsV :: [DeclV] -> (EnvV, Store) -> (EnvV, Store)
evalDeclsV [] envSto = envSto
evalDeclsV (Dim x a : decls) (envV, s@(Sto sto next)) = evalDeclsV decls (envV', sto')
  where
    envV' = updateEnvV x loc envV
    sto' = Sto (updateSto loc val sto) (new loc)
    loc = next
    val = evalAexp a envV s

evalDeclsP :: [DeclP] -> EnvV -> EnvP -> EnvP
evalDeclsP [] _ envP = envP
evalDeclsP (Proc name _ com : decls) envV envP = evalDeclsP decls envV envP'
  where
    envP' = updateEnvP name g envP
    g = evalCom com envV envP

prog0 :: Com
prog0 = Block
          [Dim "x" (Num 5), Dim "y" (Num 25)]
          []
          Skip

prog1 :: Com
prog1 = Block
					[Dim "x" (Num 15), Dim "y" (Num 0)]
					[Proc "addx" "a" (Assign "x" (Add (Var "x") (Var "x")))]
					(Call "addx" (Num 100))

main :: IO ()
main = do
  print "prog0"
  print $ evalCom prog0 [] [] (Sto [] 0)
  print "prog1"
  print $ evalCom prog1 [] [] (Sto [] 0)

