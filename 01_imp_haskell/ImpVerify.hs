module ImpVerify where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

type Numeral = Int
type Identifier = String
data Aexp = Num Numeral | Var Identifier | Add Aexp Aexp | Mul Aexp Aexp deriving (Show, Eq)
data Bexp = T | F | Not Bexp | And Bexp Bexp | Or Bexp Bexp | Eq Aexp Aexp | Leq Aexp Aexp deriving (Show, Eq)
data Com = Skip | Assign Identifier Aexp | Seq Com Com | If Bexp Com Com | While Bexp Com deriving (Show, Eq)

type State = [(Identifier, Numeral)]

lookupState :: Identifier -> State -> Maybe Numeral
lookupState = lookup

insertState :: (Identifier, Numeral) -> State -> State
insertState (x, n) [] = [(x, n)]
insertState (x, n) ((y, m) : s)
	| x == y
		= (x, n) : s
	| otherwise
		= (y, m) : (insertState (x, n) s)


data Atree = Aaxiom (Aexp, State, Numeral) | Arule (Aexp, State, Numeral) [Atree] deriving (Show, Eq)
data Btree = Baxiom (Bexp, State, Bool) | Brule (Bexp, State, Bool) [Btree] deriving (Show, Eq)
data Ctree = Caxiom (Com, State, State) | Crule (Com, State, State) [Ctree] deriving (Show, Eq)

checkAtree :: Atree -> Maybe (Aexp, State, Numeral)

checkAtree (Aaxiom a@((Num m), s, n))
	| m == n = Just a
	
checkAtree (Aaxiom a@((Var x), s, m)) = do
	n <- lookupState x s
	if n == m then
		return a
	else
		fail ""

checkAtree (Arule a@((Add a1 a2), s, m) [p1,p2]) = do
	(a1', s1, m1) <- checkAtree p1
	(a2', s2, m2) <- checkAtree p2
	if 	a1' == a1 && a2' == a2 &&
		s1 == s && s2 == s &&
		m1 + m2 == m then
		return a
	else
		fail ""

checkAtree (Arule a@((Mul a1 a2), s, m) [p1,p2]) = do
	(a1', s1, m1) <- checkAtree p1
	(a2', s2, m2) <- checkAtree p2
	if 	a1' == a1 && a2' == a2 &&
		s1 == s && s2 == s &&
		m1 * m2 == m then
		return a
	else
		fail ""

checkAtree _ = Nothing

checkBtree :: Btree -> Maybe (Bexp, State, Bool)

checkBtree (Baxiom b@(T, _, True)) = Just b
checkBtree (Baxiom b@(F, _, False)) = Just b
checkBtree (Brule b@((Not e), s, t) [p]) = do
	(e', s', t') <- checkBtree p
	if e' == e && s' == s && t' == (not t) then
		return b
	else
		fail ""



checkBtree _ = Nothing

--checkCtree :: CTree -> Maybe (Com, State, State)

-- unit tests

checkABCTreeTests :: [Test]
checkABCTreeTests =
  [ testCase "Atree: Num, correct" $
    Just ((Num 5), [], 5) @=? checkAtree (Aaxiom ((Num 5), [], 5))
  , testCase "Atree: Num, incorrect" $
  	Nothing @=? checkAtree (Aaxiom ((Num 5), [], 2))
  , testCase "Atree: Var, correct" $
  	Just ((Var "x"), [("x", 5)], 5) @=? checkAtree (Aaxiom ((Var "x"), [("x", 5)], 5))
  , testCase "Atree: Var, incorrect (other value)" $
  	Nothing @=? checkAtree (Aaxiom ((Var "x"), [("x", 1)], 5))
  , testCase "Atree: Var, incorrect (not found)" $
  	Nothing @=? checkAtree (Aaxiom ((Var "x"), [], 5))
  , testCase "Btree: T, correct" $
  	Just (T, [], True) @=? checkBtree (Baxiom (T, [], True))
  , testCase "Btree: T, incorrect" $
  	Nothing @=? checkBtree (Baxiom (T, [], False))
  , testCase "Btree: F, correct" $
  	Just (F, [], False) @=? checkBtree (Baxiom (F, [], False))
  , testCase "Btree: F, incorrect" $
  	Nothing @=? checkBtree (Baxiom (F, [], True))
  , testCase "Btree: F, correct" $
  	Just ((Not T), [], False) @=? checkBtree (Brule ((Not T), [], False) [Baxiom (T, [], True)])
  , testCase "Btree: F, incorrect" $
  	Nothing @=? checkBtree (Brule ((Not T), [], True) [Baxiom (T, [], True)])
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
