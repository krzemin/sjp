module ImpVerify where


type Identifier = String
data Aexp = Num Int | Var Identifier | Add Aexp Aexp | Mul Aexp Aexp
data Bexp = T | F | And Bexp Bexp | Or Bexp Bexp | Not Bexp | Eq Aexp Aexp | Leq Aexp Aexp
data Com = Skip | Assign Identifier Aexp | Seq Com Com | If Bexp Com Com | While Bexp Com

type State = [(Identifier, Int)]
