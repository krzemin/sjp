module ImpEnv where

import Data.Maybe
import Data.Function (fix)

type IdeV = String
type IdeP = String

data Aexp = Num Int
          | Var IdeV
          | Add Aexp Aexp
          deriving (Show, Eq)

data Bexp = T
          | F
          | Not Bexp
          | Eq Aexp Aexp
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
type EnvP = [(IdeP, Int -> Store -> Store)]

-- smart constructors for convinient infix notation
sq :: Com -> Com -> Com
sq = Seq


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

lookupEnvP :: EnvP -> IdeP -> Maybe (Int -> Store -> Store)
lookupEnvP envP p = lookup p envP

updateEnvP :: IdeP -> (Int -> Store -> Store) -> EnvP -> EnvP
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

-- simple garbage collector

gc :: Loc -> Store -> Store
gc 0 s = s
gc next (Sto sto _) = Sto sto' next
  where
    sto' = filter (\(loc, _) -> loc < next) sto

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
evalBexp (Eq a0 a1) envV sto = evalAexp a0 envV sto == evalAexp a1 envV sto

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
evalCom (Block declsV declsP c) envV envP sto@(Sto _ next) =
  gc next $ evalCom c envV' envP' sto'
    where
      (envV', sto') = evalDeclsV declsV (envV, sto)
      envP' = evalDeclsP declsP envV' envP
evalCom (Call p a) envV envP sto = procSem n sto
  where
    procSem = fromJust $ lookupEnvP envP p
    n = evalAexp a envV sto

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
evalDeclsP (Proc name arg com : decls) envV envP = evalDeclsP decls envV envP'
  where
    envP' = updateEnvP name (fix f) envP
    f g n (Sto sto next) = gc next $ evalCom com envV' (updateEnvP name g envP) (Sto sto' next')
      where
        sto' = updateSto argLoc n sto
        next' = new next
        argLoc = next
        envV' = updateEnvV arg argLoc envV


prog0 :: Com
prog0 = Block
          [Dim "x" (Num 5), Dim "y" (Num 25)]
          []
          Skip

prog1 :: Com
prog1 = Block
          [Dim "x" (Num 15)]
          [Proc "addx" "a" (Assign "x" (Add (Var "a") (Var "x")))]
          (Call "addx" (Num 100))

prog2 :: Com
prog2 = Block
          [Dim "x" (Num 0)]
          [Proc "twice_to_x" "a" (Assign "x" (Add (Var "a") (Var "a")))]
          (Call "twice_to_x" (Num 100))

prog3 :: Com
prog3 = Block
          [Dim "x" (Num 0)]
          [Proc "p" "_" (Assign "x" (Add (Var "x") (Num 1))),
           Proc "q" "_" (Call "p" (Num 0))]
          (Block
            []
            [Proc "p" "_" (Assign "x" (Num 7))]
            (Call "q" (Num 0))
            )

prog4 :: Com
prog4 = Block
          [Dim "x" (Num 0)]
          [Proc "f" "n" (If
              (Eq (Var "n") (Num 0))
              Skip
              (Assign "x" (Add (Var "x") (Num 1)) `sq`
               Call "f" (Add (Var "n") (Num (-1))))
            )]
          (Call "f" (Num 50))

prog5 :: Com
prog5 = Block
          [Dim "x" (Num 1)]
          [Proc "mulxby" "y" (Block
            [Dim "t" (Var "x")]
            [Proc "aux" "y'" (If
                (Leq (Var "y'") (Num 1))
                (Assign "x" (Var "t"))
                (Assign "t" (Add (Var "t") (Var "x")) `sq`
                 Call "aux" (Add (Var "y'") (Num (-1))))
              )]
            (Call "aux" (Var "y"))
            ),
          Proc "fact" "n" (If
                (Eq (Var "n") (Num 0))
                Skip
                (Call "mulxby" (Var "n") `sq`
                 Call "fact" (Add (Var "n") (Num (-1))))
                )]
          (Call "fact" (Num 15))


main :: IO ()
main = do
  print "prog0 -- block declarations"
  print $ evalCom prog0 [] [] (Sto [] 0)
  print "prog1 -- procedure with parameter"
  print $ evalCom prog1 [] [] (Sto [] 0)
  print "prog2 -- procedure with parameter"
  print $ evalCom prog2 [] [] (Sto [] 0)
  print "prog3 -- nested blocks (procedure binding)"
  print $ evalCom prog3 [] [] (Sto [] 0)
  print "prog4 -- simple recursion"
  print $ evalCom prog4 [] [] (Sto [] 0)
  print "prog5 -- advanced recursion (factorial)"
  print $ evalCom prog5 [] [] (Sto [] 0)

