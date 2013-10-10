module ImpVerify where

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
	| x == y = (x, n) : s
	| otherwise = (y, m) : (insertState (x, n) s)

