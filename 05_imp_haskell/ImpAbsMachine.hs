module ImpAbsMachine where

import ImpAst
import ImpMemory


data StackElem = Add1 Aexp | Add0 Numeral

stepMachine :: (Aexp, [StackElem], State) -> (Aexp, [StackElem], State)

stepMachine (Var x, st, s) = (Num (fromJust $ lookupState x s), st, s)

stepMachine (Num n1, (Add0 n0):st, s) = (Num (n0 + n1), st, s)

stepMachine (Num n0, (Add1 a1):st, s) = (a1, (Add0 n0):st, s)









