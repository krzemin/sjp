module ImpMemory where

import ImpAst

type State = [(Identifier, Numeral)]

lookupState :: Identifier -> State -> Maybe Numeral
lookupState = lookup

insertState :: (Identifier, Numeral) -> State -> State
insertState (x, n) lst = (x, n) : lst
