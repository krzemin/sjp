module ImpAst where

type Numeral = Int
type Identifier = String

data Aexp = Num Numeral
          | Var Identifier
          | Add Aexp Aexp
          deriving (Show, Eq)

data Bexp = T
          | F
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Leq Aexp Aexp
          deriving (Show, Eq)

data Com = Skip
          | Assign Identifier Aexp
          | Seq Com Com
          | If Bexp Com Com
          | While Bexp Com
          deriving (Show, Eq)
