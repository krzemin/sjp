module ImpVerify where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

type Numeral = Int
type Identifier = String
data Aexp = Num Numeral | Var Identifier | Add Aexp Aexp | Mul Aexp Aexp deriving (Show, Eq)
data Bexp = T | F | And Bexp Bexp | Or Bexp Bexp | Not Bexp | Eq Aexp Aexp | Leq Aexp Aexp deriving (Show, Eq)
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


data Atree = Axiom (Aexp, State, Numeral) | Rule (Aexp, State, Numeral) [Atree]
--data Btree =
--data Ctree =

checkAtree :: Atree -> Maybe (Aexp, State, Numeral)
checkAtree (Axiom a@((Num m), s, n))
	| m == n
		= Just a
	| otherwise
		= Nothing

checkAtree (Axiom a@((Var x), s, m)) = do
	n <- lookupState x s
	if n == m then
		return a
	else
		fail ""

checkAtree (Rule a@((Add a1 a2), s, m) [p1,p2]) = do
	(a1', s1, m1) <- checkAtree p1
	(a2', s2, m2) <- checkAtree p2
	if 	a1' == a1 && a2' == a2 &&
		s1 == s && s2 == s &&
		m1 + m2 == m then
		return a
	else
		fail ""

checkAtree (Rule a@((Mul a1 a2), s, m) [p1,p2]) = do
	(a1', s1, m1) <- checkAtree p1
	(a2', s2, m2) <- checkAtree p2
	if 	a1' == a1 && a2' == a2 &&
		s1 == s && s2 == s &&
		m1 * m2 == m then
		return a
	else
		fail ""

--checkBTree :: Btree -> Maybe (Bexp, State, Bool)
--checkCtree :: CTree -> Maybe (Com, State, State)

-- unit tests

checkABCTreeTests :: [Test]
checkABCTreeTests =
  [ testCase "Atree: Num, correct" $
    Just ((Num 5), [], 5) @=? checkAtree (Axiom ((Num 5), [], 5))
  , testCase "Atree: Num, incorrect" $
  	Nothing @=? checkAtree (Axiom ((Num 5), [], 2))
  , testCase "Atree: Var, correct" $
  	Just ((Var "x"), [("x", 5)], 5) @=? checkAtree (Axiom ((Var "x"), [("x", 5)], 5))
  , testCase "Atree: Var, incorrect (other value)" $
  	Nothing @=? checkAtree (Axiom ((Var "x"), [("x", 1)], 5))
  , testCase "Atree: Var, incorrect (not found)" $
  	Nothing @=? checkAtree (Axiom ((Var "x"), [], 5))
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
