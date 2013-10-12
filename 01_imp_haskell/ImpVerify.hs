module ImpVerify where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Debug.Trace 

type Numeral = Int
type Identifier = String
data Aexp = Num Numeral | Var Identifier | Add Aexp Aexp | Mul Aexp Aexp deriving (Show, Eq)
data Bexp = T | F | Not Bexp | And Bexp Bexp | Or Bexp Bexp | Leq Aexp Aexp deriving (Show, Eq)
data Com = Skip | Assign Identifier Aexp | Seq Com Com | If Bexp Com Com | While Bexp Com deriving (Show, Eq)

type State = [(Identifier, Numeral)]

lookupState :: Identifier -> State -> Maybe Numeral
lookupState = lookup

insertState :: (Identifier, Numeral) -> State -> State
insertState (x, n) [] = [(x, n)]
insertState (x, n) ((y, m) : s)
	| x == y = (x, n) : s
	| otherwise = (y, m) : (insertState (x, n) s)


data Atree =  NumAxiom (Aexp, State, Numeral)
            | VarAxiom (Aexp, State, Numeral)
            | AddRule (Aexp, State, Numeral) Atree Atree
            | MulRule (Aexp, State, Numeral) Atree Atree

checkAtree :: Atree -> Maybe (Aexp, State, Numeral)

checkAtree (NumAxiom a@(Num m, _, n))
  | m == n = Just a

checkAtree (VarAxiom a@(Var x, s, n)) = do
  m <- lookupState x s
  case m == n of
    True -> return a
    otherwise -> fail ""
  
checkAtree (AddRule a@(Add e1 e2, s, n) tree1 tree2) = do
  (e1', s1', n1') <- checkAtree tree1
  (e2', s2', n2') <- checkAtree tree2
  case (n, e1, e2, s, s) == (n1' + n2', e1', e2', s1', s2') of
    True -> return a
    otherwise -> fail ""

checkAtree (MulRule a@(Mul e1 e2, s, n) tree1 tree2) = do
  (e1', s1', n1') <- checkAtree tree1
  (e2', s2', n2') <- checkAtree tree2
  case (n, e1, e2, s, s) == (n1' * n2', e1', e2', s1', s2') of
    True -> return a
    otherwise -> fail ""

checkAtree _ = Nothing


data Btree =  TrueAxiom (Bexp, State, Bool)
            | FalseAxiom (Bexp, State, Bool)
            | LeqRule (Bexp, State, Bool) Atree Atree
            | NotRule (Bexp, State, Bool) Btree
            | AndRule (Bexp, State, Bool) Btree Btree
            | OrRule (Bexp, State, Bool) Btree Btree

checkBtree :: Btree -> Maybe (Bexp, State, Bool)

checkBtree (TrueAxiom b@(T, _, True)) = Just b

checkBtree (FalseAxiom b@(F, _, False)) = Just b

checkBtree (LeqRule b@(Leq e1 e2, s, r) tree1 tree2) = do
  (e1', s1', n1') <- checkAtree tree1
  (e2', s2', n2') <- checkAtree tree2
  case (r, e1, e2, s, s) == (n1' <= n2', e1', e2', s1', s2') of
    True -> return b
    otherwise -> fail ""

checkBtree (NotRule b@(Not e, s, r) tree) = do
  (e', s', r') <- checkBtree tree
  case (r, e, s) == (not r', e', s') of
    True -> return b
    otherwise -> fail ""

checkBtree (AndRule b@(And e1 e2, s, r) tree1 tree2) = do
  (e1', s1', r1') <- checkBtree tree1
  (e2', s2', r2') <- checkBtree tree2
  case (r, e1, e2, s, s) == (r1' && r2', e1', e2', s1', s2') of
    True -> return b
    otherwise -> fail ""

checkBtree (OrRule b@(Or e1 e2, s, r) tree1 tree2) = do
  (e1', s1', r1') <- checkBtree tree1
  (e2', s2', r2') <- checkBtree tree2
  case (r, e1, e2, s, s) == (r1' || r2', e1', e2', s1', s2') of
    True -> return b
    otherwise -> fail ""

checkBtree _ = Nothing



--checkCtree :: CTree -> Maybe (Com, State, State)

-- unit tests

checkABCTreeTests :: [Test]
checkABCTreeTests =
  [ testCase "Atree: Num, incorrect" $
    Nothing @=?
    checkAtree (NumAxiom (Num 1, [], 0))
  , testCase "Atree: Num, correct" $
    Just (Num 1, [], 1) @=?
    checkAtree (NumAxiom (Num 1, [], 1))

  , testCase "Atree: Var, incorrect 1" $
    Nothing @=?
    checkAtree (VarAxiom (Var "x", [], 1))
  , testCase "Atree: Var, incorrect 2" $
    Nothing @=?
    checkAtree (VarAxiom (Var "x", [("x", 2)], 1))
  , testCase "Atree: Var, correct" $
    Just (Var "x", [("x", 2)], 2) @=?
    checkAtree (VarAxiom (Var "x", [("x", 2)], 2))
  
  , testCase "Atree: Add, incorrect 1" $
    let expr = Add (Num 2) (Num 4) in
    Nothing @=?
    checkAtree (AddRule (expr, [], 8) (NumAxiom (Num 2, [], 2)) (NumAxiom (Num 4, [], 4)))
  , testCase "Atree: Add, incorrect 2" $
    let expr = Add (Num 2) (Num 4) in
    Nothing @=?
    checkAtree (AddRule (expr, [], 6) (NumAxiom (Num 3, [], 3)) (NumAxiom (Num 3, [], 3)))
  , testCase "Atree: Add, incorrect 3" $
    let expr = Add (Var "x") (Num 2) in
    let state = [("x", 4)] in
    Nothing @=?
    checkAtree (AddRule (expr, state, 6) (NumAxiom (Var "y", [("y", 4)], 4)) (NumAxiom (Num 2, state, 2)))
  , testCase "Atree: Add, correct" $
    let expr = Add (Num 2) (Var "x") in
    let state = [("x", 4)] in
    Just (expr, state, 6) @=?
    checkAtree (AddRule (expr, state, 6) (NumAxiom (Num 2, state, 2)) (VarAxiom (Var "x", state, 4)))

  , testCase "Atree: Mul, incorrect 1" $
    let expr = Mul (Num 2) (Num 4) in
    Nothing @=?
    checkAtree (MulRule (expr, [], 3) (NumAxiom (Num 2, [], 2)) (NumAxiom (Num 4, [], 4)))
  , testCase "Atree: Mul, incorrect 2" $
    let expr = Mul (Num 2) (Num 4) in
    Nothing @=?
    checkAtree (MulRule (expr, [], 6) (NumAxiom (Num 3, [], 3)) (NumAxiom (Num 3, [], 3)))
  , testCase "Atree: Mul, incorrect 3" $
    let expr = Mul (Var "x") (Num 2) in
    let state = [("x", 4)] in
    Nothing @=?
    checkAtree (MulRule (expr, state, 6) (NumAxiom (Var "y", [("y", 4)], 4)) (NumAxiom (Num 2, state, 2)))
  , testCase "Atree: Mul, correct" $
    let expr = Mul (Num 2) (Var "x") in
    let state = [("x", 4)] in
    Just (expr, state, 8) @=?
    checkAtree (MulRule (expr, state, 8) (NumAxiom (Num 2, state, 2)) (VarAxiom (Var "x", state, 4)))

  , testCase "Btree: T, incorrect" $
    Nothing @=?
    checkBtree (TrueAxiom (T, [], False))
  , testCase "Btree: T, correct" $
    Just (T, [], True) @=?
    checkBtree (TrueAxiom (T, [], True))

  , testCase "Btree: F, incorrect" $
    Nothing @=?
    checkBtree (FalseAxiom (F, [], True))
  , testCase "Btree: F, correct" $
    Just (F, [], False) @=?
    checkBtree (FalseAxiom (F, [], False))

  , testCase "Btree: Leq, incorrect 1" $
    Nothing @=?
    checkBtree (LeqRule (Leq (Num 2) (Num 3), [], False) (NumAxiom (Num 2, [], 2)) (NumAxiom (Num 3, [], 3)))
  , testCase "Btree: Leq, incorrect 2" $
    Nothing @=?
    checkBtree (LeqRule (Leq (Num 3) (Num 3), [], False) (NumAxiom (Num 3, [], 3)) (NumAxiom (Num 3, [], 3)))
  , testCase "Btree: Leq, incorrect 3" $
    Nothing @=?
    checkBtree (LeqRule (Leq (Num 4) (Num 3), [], True) (NumAxiom (Num 4, [], 4)) (NumAxiom (Num 3, [], 3)))
  , testCase "Btree: Leq, incorrect 4" $
    let state = [("x", 5)] in
    Nothing @=?
    checkBtree (LeqRule (Leq (Var "x") (Num 8), state, True) (VarAxiom (Var "y", state, 3)) (NumAxiom (Num 8, [], 8)))
  , testCase "Btree: Leq, incorrect 5" $
    let state = [("x", 5)] in
    Nothing @=?
    checkBtree (LeqRule (Leq (Var "x") (Num 8), state, True) (VarAxiom (Var "x", [("x", 1)], 1)) (NumAxiom (Num 8, [], 8)))
  , testCase "Btree: Leq, correct 1" $
    Just (Leq (Num 2) (Num 3), [], True) @=?
    checkBtree (LeqRule (Leq (Num 2) (Num 3), [], True) (NumAxiom (Num 2, [], 2)) (NumAxiom (Num 3, [], 3)))
  , testCase "Btree: Leq, correct 2" $
    Just (Leq (Num 3) (Num 3), [], True) @=?
    checkBtree (LeqRule (Leq (Num 3) (Num 3), [], True) (NumAxiom (Num 3, [], 3)) (NumAxiom (Num 3, [], 3)))
  , testCase "Btree: Leq, correct 3" $
    let state = [("x", 9)] in
    Just (Leq (Var "x") (Num 3), state, False) @=?
    checkBtree (LeqRule (Leq (Var "x") (Num 3), state, False) (VarAxiom (Var "x", state, 9)) (NumAxiom (Num 3, state, 3)))

  , testCase "Btree: Not, incorrect 1" $
    Nothing @=?
    checkBtree (NotRule (Not T, [], True) (TrueAxiom (T, [], True)))
  , testCase "Btree: Not, incorrect 2" $
    Nothing @=?
    checkBtree (NotRule (Not T, [], False) (TrueAxiom (T, [("x", 1)], True)))
  , testCase "Btree: Not, correct" $
    Just (Not F, [], True) @=?
    checkBtree (NotRule (Not F, [], True) (FalseAxiom (F, [], False)))

  , testCase "Btree: And, incorrect 1" $
    let expr = And T T in
    Nothing @=?
    checkBtree (AndRule (expr, [], False) (TrueAxiom (T, [], True)) (TrueAxiom (T, [], True)))
  , testCase "Btree: And, incorrect 2" $
    let expr = And T T in
    Nothing @=?
    checkBtree (AndRule (expr, [], True) (FalseAxiom (F, [], False)) (TrueAxiom (T, [], True)))
  , testCase "Btree: And, incorrect 3" $
    let expr = And T T in
    Nothing @=?
    checkBtree (AndRule (expr, [], True) (TrueAxiom (T, [], True)) (TrueAxiom (T, [("x", 2)], True)))
  , testCase "Btree: And, correct" $
    let expr = And T T in
    Just (expr, [], True)  @=?
    checkBtree (AndRule (expr, [], True) (TrueAxiom (T, [], True)) (TrueAxiom (T, [], True)))

  , testCase "Btree: Or, incorrect 1" $
    let expr = Or F T in
    Nothing @=?
    checkBtree (OrRule (expr, [], False) (FalseAxiom (F, [], False)) (TrueAxiom (T, [], True)))
  , testCase "Btree: Or, incorrect 2" $
    let expr = Or T T in
    Nothing @=?
    checkBtree (OrRule (expr, [], True) (TrueAxiom (T, [], True)) (FalseAxiom (F, [], False)))
  , testCase "Btree: Or, incorrect 3" $
    let expr = Or F F in
    Nothing @=?
    checkBtree (OrRule (expr, [], False) (FalseAxiom (F, [], False)) (FalseAxiom (F, [("x", 2)], False)))
  , testCase "Btree: Or, correct" $
    let expr = Or F T in
    Just (expr, [], True)  @=?
    checkBtree (OrRule (expr, [], True) (FalseAxiom (F, [], False)) (TrueAxiom (T, [], True)))

  ]

-- testing stuff

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

main :: IO ()
main = exitProperly (runTestTT (TestList checkABCTreeTests))
