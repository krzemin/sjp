-- Piotr Krzemiński, Semantyka Języków Programowania, 2013/14
-- Lista 1, zad. 2

module ImpVerify where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

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


(==>) :: Bool -> a -> Maybe a
False ==> _ = Nothing
True ==> v = Just v

data Atree =  NumAxiom (Aexp, State, Numeral)
            | VarAxiom (Aexp, State, Numeral)
            | AddRule (Aexp, State, Numeral) Atree Atree
            | MulRule (Aexp, State, Numeral) Atree Atree

checkAtree :: Atree -> Maybe (Aexp, State, Numeral)

checkAtree (NumAxiom a@(Num m, _, n)) = do
  (m == n) ==> a

checkAtree (VarAxiom a@(Var x, s, n)) = do
  m <- lookupState x s
  (m == n) ==> a
  
checkAtree (AddRule a@(Add e1 e2, s, n) tree1 tree2) = do
  (e1', s1', n1') <- checkAtree tree1
  (e2', s2', n2') <- checkAtree tree2
  ((n, e1, e2, s, s) == (n1' + n2', e1', e2', s1', s2')) ==> a

checkAtree (MulRule a@(Mul e1 e2, s, n) tree1 tree2) = do
  (e1', s1', n1') <- checkAtree tree1
  (e2', s2', n2') <- checkAtree tree2
  ((n, e1, e2, s, s) == (n1' * n2', e1', e2', s1', s2')) ==> a

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
  ((r, e1, e2, s, s) == (n1' <= n2', e1', e2', s1', s2')) ==> b

checkBtree (NotRule b@(Not e, s, r) tree) = do
  (e', s', r') <- checkBtree tree
  ((r, e, s) == (not r', e', s')) ==> b

checkBtree (AndRule b@(And e1 e2, s, r) tree1 tree2) = do
  (e1', s1', r1') <- checkBtree tree1
  (e2', s2', r2') <- checkBtree tree2
  ((r, e1, e2, s, s) == (r1' && r2', e1', e2', s1', s2')) ==> b

checkBtree (OrRule b@(Or e1 e2, s, r) tree1 tree2) = do
  (e1', s1', r1') <- checkBtree tree1
  (e2', s2', r2') <- checkBtree tree2
  ((r, e1, e2, s, s) == (r1' || r2', e1', e2', s1', s2')) ==> b

checkBtree _ = Nothing


data Ctree =  SkipAxiom (Com, State, State)
            | AssignRule (Com, State, State) Atree
            | SeqRule (Com, State, State) Ctree Ctree
            | IfRuleT (Com, State, State) Btree Ctree
            | IfRuleF (Com, State, State) Btree Ctree
            | WhileRuleF (Com, State, State) Btree
            | WhileRuleT (Com, State, State) Btree Ctree Ctree

checkCtree :: Ctree -> Maybe (Com, State, State)

checkCtree (SkipAxiom c@(Skip, s1, s2))
  | s1 == s2 = Just c

checkCtree (AssignRule c@(Assign x e, s1, s2) tree) = do
  (e', s', n') <- checkAtree tree
  let s'' = insertState (x, n') s'
  ((e, s1, s2) == (e', s', s'')) ==> c

checkCtree (SeqRule c@(Seq c1 c2, s1, s3) tree1 tree2) = do
  (c1', s1', s2') <- checkCtree tree1
  (c2', s2'', s3'') <- checkCtree tree2
  ((c1, c2, s1, s2', s3) == (c1', c2', s1', s2'', s3'')) ==> c

checkCtree (IfRuleT c@(If b com _, s1, s2) tree1 tree2) = do
  (b', s1', r') <- checkBtree tree1
  (com', s1'', s2'') <- checkCtree tree2
  ((r', b, com, s1, s1, s2) == (True, b', com', s1', s1'', s2'')) ==> c

checkCtree (IfRuleF c@(If b _ com, s1, s2) tree1 tree2) = do
  (b', s1', r') <- checkBtree tree1
  (com', s1'', s2'') <- checkCtree tree2
  ((r', b, com, s1, s1, s2) == (False, b', com', s1', s1'', s2'')) ==> c

checkCtree (WhileRuleF c@(While b _, s1, s2) tree) = do
  (b', s1', r') <- checkBtree tree
  ((r', b, s1, s1) == (False, b', s1', s2)) ==> c

checkCtree (WhileRuleT c@(While b com, s1, s3) tree1 tree2 tree3) = do
  (b', s1', r') <- checkBtree tree1
  (com'', s1'', s2'') <- checkCtree tree2
  (while''', s2''', s3''') <- checkCtree tree3
  ((r', b, s1, com, s1, While b com, s2'', s3) == (True, b', s1', com'', s1'', while''', s2''', s3''')) ==> c

checkCtree _ = Nothing

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

  , testCase "Ctree: Skip, incorrect" $
    Nothing @=?
    checkCtree (SkipAxiom (Skip, [], [("x", 2)]))
  , testCase "Ctree: Skip, correct" $
    Just (Skip, [("x", 2)], [("x", 2)]) @=?
    checkCtree (SkipAxiom (Skip, [("x", 2)], [("x", 2)]))

  , testCase "Ctree: Assign, incorrect 1" $
    Nothing @=?
    checkCtree (AssignRule (Assign "x" (Num 2), [], [("x", 3)]) (NumAxiom (Num 2, [], 2)))
  , testCase "Ctree: Assign, incorrect 2" $
    Nothing @=?
    checkCtree (AssignRule (Assign "x" (Num 2), [], [("x", 3)]) (NumAxiom (Num 3, [], 3)))
  , testCase "Ctree: Assign, correct" $
    Just (Assign "x" (Num 2), [], [("x", 2)]) @=?
    checkCtree (AssignRule (Assign "x" (Num 2), [], [("x", 2)]) (NumAxiom (Num 2, [], 2)))

  , testCase "Ctree: Seq, incorrect 1" $
    let com1 = Assign "x" (Num 2) in
    let com2 = Assign "y" (Num 1) in
    let s1 = [] in
    let s2 = [("x", 2)] in
    let s3 = [("x", 2), ("y", 1)] in
    let s3' = [("x", 2), ("y", 0)] in
    let tree1 = (AssignRule (com1, s1, s2) (NumAxiom (Num 2, [], 2))) in
    let tree2 = (AssignRule (com2, s2, s3) (NumAxiom (Num 1, [], 1))) in
    Nothing @=?
    checkCtree (SeqRule (Seq com1 com2, s1, s3') tree1 tree2)
  , testCase "Ctree: Seq, incorrect 2" $
    let com1 = Assign "x" (Num 2) in
    let com2 = Assign "y" (Num 1) in
    let s1 = [] in
    let s2 = [("x", 2)] in
    let s3 = [("x", 2), ("y", 1)] in
    let tree1 = (AssignRule (com1, s1, s2) (NumAxiom (Num 2, [], 2))) in
    let tree2 = (AssignRule (Assign "y" (Num 5), s2, s3) (NumAxiom (Num 5, [], 5))) in
    Nothing @=?
    checkCtree (SeqRule (Seq com1 com2, s1, s3) tree1 tree2)
  , testCase "Ctree: Seq, incorrect 3" $
    let com1 = Assign "x" (Num 2) in
    let com2 = Assign "y" (Num 1) in
    let s1 = [] in
    let s2 = [("x", 2)] in
    let s3 = [("x", 2), ("y", 1)] in
    let tree1 = (AssignRule (com1, [("x", 0)], s2) (NumAxiom (Num 2, [], 2))) in
    let tree2 = (AssignRule (com2, s2, s3) (NumAxiom (Num 1, [], 1))) in
    Nothing @=?
    checkCtree (SeqRule (Seq com1 com2, s1, s3) tree1 tree2)
  , testCase "Ctree: Seq, incorrect 4" $
    let com1 = Assign "x" (Num 2) in
    let com2 = Assign "y" (Num 1) in
    let s1 = [] in
    let s2 = [("x", 2)] in
    let s3 = [("x", 2), ("y", 1)] in
    let tree1 = (AssignRule (com1, s1, s2) (NumAxiom (Num 2, [], 2))) in
    let tree2 = (AssignRule (com2, [("x",2),("y", 0)], s3) (NumAxiom (Num 1, [], 1))) in
    Nothing @=?
    checkCtree (SeqRule (Seq com1 com2, s1, s3) tree1 tree2)
  , testCase "Ctree: Seq, correct 1" $
    let com1 = Assign "x" (Num 2) in
    let com2 = Assign "y" (Num 1) in
    let s1 = [] in
    let s2 = [("x", 2)] in
    let s3 = [("x", 2), ("y", 1)] in
    let tree1 = (AssignRule (com1, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    let tree2 = (AssignRule (com2, s2, s3) (NumAxiom (Num 1, s2, 1))) in
    Just (Seq com1 com2, s1, s3) @=?
    checkCtree (SeqRule (Seq com1 com2, s1, s3) tree1 tree2)
  , testCase "Ctree: Seq, correct 2" $
    let aexp = Add (Var "x") (Num 1) in
    let com = Assign "x" aexp in
    let s1 = [("x", 0)] in
    let s2 = [("x", 1)] in
    let s3 = [("x", 2)] in
    let tree1 = (AssignRule (com, s1, s2) (AddRule (aexp, s1, 1) (VarAxiom (Var "x", s1, 0)) (NumAxiom (Num 1, s1, 1)))) in
    let tree2 = (AssignRule (com, s2, s3) (AddRule (aexp, s2, 2) (VarAxiom (Var "x", s2, 1)) (NumAxiom (Num 1, s2, 1)))) in
    Just (Seq com com, s1, s3) @=?
    checkCtree (SeqRule (Seq com com, s1, s3) tree1 tree2)

  , testCase "Ctree: IfRuleT, incorrect 1" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T com Skip, s1, s1) tree1 tree2)
  , testCase "Ctree: IfRuleT, incorrect 2" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T Skip com, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleT, incorrect 3" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (Assign "x" (Num 3), s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T com Skip, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleT, incorrect 4" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (Assign "x" (Num 2), s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T com Skip, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleT, incorrect 5" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (com, [], s2) (NumAxiom (Num 2, [], 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T com Skip, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleT, incorrect 6" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, [], True)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleT (If T com Skip, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleT, correct" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Just (If T com Skip, s1, s2) @=?
    checkCtree (IfRuleT (If T com Skip, s1, s2) tree1 tree2)

  , testCase "Ctree: IfRuleF, incorrect 1" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F Skip com, s1, s1) tree1 tree2)
  , testCase "Ctree: IfRuleF, incorrect 2" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F com Skip, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleF, incorrect 3" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (Assign "x" (Num 3), s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F Skip com, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleF, incorrect 4" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (TrueAxiom (T, s1, True)) in
    let tree2 = (AssignRule (Assign "x" (Num 2), s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F Skip com , s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleF, incorrect 5" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (com, [], s2) (NumAxiom (Num 2, [], 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F Skip com, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleF, incorrect 6" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, [], False)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Nothing @=?
    checkCtree (IfRuleF (If F Skip com, s1, s2) tree1 tree2)
  , testCase "Ctree: IfRuleF, correct" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    let tree2 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    Just (If F Skip com, s1, s2) @=?
    checkCtree (IfRuleF (If F Skip com, s1, s2) tree1 tree2)

  , testCase "Ctree: WhileRuleF, incorrect 1" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    Nothing @=?
    checkCtree (WhileRuleF (While F com, s1, s2) tree1)
  , testCase "Ctree: WhileRuleF, incorrect 2" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, [], False)) in
    Nothing @=?
    checkCtree (WhileRuleF (While F com, s1, s1) tree1)
  , testCase "Ctree: WhileRuleF, correct" $
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let tree1 = (FalseAxiom (F, s1, False)) in
    Just (While F com, s1, s1) @=?
    checkCtree (WhileRuleF (While F com, s1, s1) tree1)

  , testCase "Ctree: WhileRuleT, incorrect 1" $
    let b = (Leq (Var "x") (Num 0)) in
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let btree1 = (LeqRule (T, s1, True) (VarAxiom (Var "x", s1, 0)) (NumAxiom (Num 0, s1, 0))) in
    let btree2 = (LeqRule (b, s2, False) (VarAxiom (Var "x", s2, 2)) (NumAxiom (Num 0, s2, 0))) in
    let ctree1 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    let ctree2 = (WhileRuleF (While b com, s2, s2) btree2) in
    Nothing @=?
    checkCtree (WhileRuleT (While b com, s1, s2) btree1 ctree1 ctree2)
  , testCase "Ctree: WhileRuleT, incorrect 2" $
    let b = (Leq (Var "x") (Num 0)) in
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s1' = [("x", -1)] in
    let s2 = [("x", 2)] in
    let btree1 = (LeqRule (b, s1', True) (VarAxiom (Var "x", s1', -1)) (NumAxiom (Num 0, s1', 0))) in
    let btree2 = (LeqRule (b, s2, False) (VarAxiom (Var "x", s2, 2)) (NumAxiom (Num 0, s2, 0))) in
    let ctree1 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    let ctree2 = (WhileRuleF (While b com, s2, s2) btree2) in
    Nothing @=?
    checkCtree (WhileRuleT (While b com, s1, s2) btree1 ctree1 ctree2)
  , testCase "Ctree: WhileRuleT, incorrect 3" $
    let b = (Leq (Var "x") (Num 0)) in
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let btree1 = (LeqRule (b, s1, True) (VarAxiom (Var "x", s1, 0)) (NumAxiom (Num 0, s1, 0))) in
    let btree2 = (LeqRule (b, s2, False) (VarAxiom (Var "x", s2, 2)) (NumAxiom (Num 0, s2, 0))) in
    let ctree1 = (SkipAxiom (Skip, s2, s2)) in
    let ctree2 = (WhileRuleF (While b Skip, s2, s2) btree2) in
    Nothing @=?
    checkCtree (WhileRuleT (While b com, s1, s2) btree1 ctree1 ctree2)
  , testCase "Ctree: WhileRuleT, correct" $
    let b = (Leq (Var "x") (Num 0)) in
    let com = Assign "x" (Num 2) in
    let s1 = [("x", 0)] in
    let s2 = [("x", 2)] in
    let btree1 = (LeqRule (b, s1, True) (VarAxiom (Var "x", s1, 0)) (NumAxiom (Num 0, s1, 0))) in
    let btree2 = (LeqRule (b, s2, False) (VarAxiom (Var "x", s2, 2)) (NumAxiom (Num 0, s2, 0))) in
    let ctree1 = (AssignRule (com, s1, s2) (NumAxiom (Num 2, s1, 2))) in
    let ctree2 = (WhileRuleF (While b com, s2, s2) btree2) in
    Just (While b com, s1, s2) @=?
    checkCtree (WhileRuleT (While b com, s1, s2) btree1 ctree1 ctree2)
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
