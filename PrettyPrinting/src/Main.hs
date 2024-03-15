module Main where

-- Make sure you run this via `stack run` in the top-level folder.
-- If you run it from `src` or `test` the tests won't be able to 
-- locate the files.

import Syntax

-- | Test case for .Length
exp1 :: Expression
exp1 = Op1 Len (Val (IntVal 5))

exp2 :: Expression
exp2 = Op2 (Val (IntVal 1)) Plus (Val (IntVal 1))

exp3 :: Expression 
exp3 = Op2 (Val (IntVal 1)) Gt (Val (IntVal 0))

exp4 :: Expression
exp4 = Op2 (Val (IntVal 2)) Plus (Val (IntVal 2))

-- | Testing Var
var1 :: Var
var1 = Name "dog"

proj1 :: Var
proj1 = Proj (Var (Name "a")) (Var (Name "i"))

-- | Testing Statements
decl1 :: Statement
decl1 = Decl ("x", TInt) (exp2)

decl2 :: Statement
decl2 = Decl ("y", TInt) (exp4)

if1 :: Statement
if1 = If exp3 block1 block2

if2 :: Statement
if2 = If exp3 block1 (Block [])

block1 :: Block
block1 = Block [decl1]

block2 :: Block 
block2 = Block [decl2]

bindings1 :: [Binding]
bindings1 = [("i", TInt), ("x", TInt), ("y", TInt)]

pred1 :: Predicate
pred1 = Predicate bindings1 exp3

while1 :: Statement
while1 = While [pred1] exp1 block1

loopToZeroCorrect :: Method
loopToZeroCorrect = Method "LoopToZero" [("m",TInt),("p",TInt)] [("x",TInt),("z",TInt)] [Requires (Predicate [] (Val (BoolVal True))),Ensures (Predicate [] (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "p")) Minus (Var (Name "m")))))] (Block [Assign (Name "x") (Var (Name "m")),Empty,Assign (Name "z") (Var (Name "p")),Empty,While [Predicate [] (Op2 (Op2 (Var (Name "z")) Minus (Var (Name "x"))) Eq (Op2 (Var (Name "p")) Minus (Var (Name "m"))))] (Op2 (Var (Name "x")) Gt (Val (IntVal 0))) (Block [Assign (Name "z") (Op2 (Var (Name "z")) Minus (Val (IntVal 1))),Empty,Assign (Name "x") (Op2 (Var (Name "x")) Minus (Val (IntVal 1))),Empty])])

intDiv :: Method
intDiv = Method "IntDiv" [("m",TInt),("n",TInt)] [("d",TInt),("r",TInt)] [Requires (Predicate [] (Op2 (Var (Name "n")) Gt (Val (IntVal 0)))),Ensures (Predicate [] (Op2 (Var (Name "m")) Eq (Op2 (Op2 (Var (Name "d")) Times (Var (Name "n"))) Plus (Var (Name "r"))))),Ensures (Predicate [] (Op2 (Op2 (Val (IntVal 0)) Le (Op2 (Var (Name "r")) Conj (Var (Name "r")))) Lt (Var (Name "n"))))] (Block [Assign (Name "d") (Op2 (Var (Name "m")) Divide (Var (Name "n"))),Empty,Assign (Name "r") (Op2 (Var (Name "m")) Modulo (Var (Name "n"))),Empty])

manyBinops :: Method
manyBinops = Method "ManyBinops" [("x",TInt),("y",TInt),("a",TBool),("b",TBool)] [("c",TInt)] [Requires (Predicate [] (Op2 (Var (Name "y")) Neq (Val (IntVal 0))))] (Block [If (Op2 (Op2 (Op2 (Var (Name "a")) Conj (Var (Name "b"))) Disj (Op2 (Op2 (Op1 Not (Var (Name "a"))) Conj (Var (Name "x"))) Lt (Var (Name "y")))) Disj (Op2 (Var (Name "x")) Ge (Var (Name "y")))) (Block [Assign (Name "c") (Op2 (Op2 (Var (Name "x")) Plus (Var (Name "y"))) Minus (Op2 (Var (Name "x")) Divide (Var (Name "y")))),Empty]) (Block [Assign (Name "c") (Op2 (Op2 (Var (Name "x")) Times (Var (Name "y"))) Modulo (Var (Name "y"))),Empty])])

req :: Method 
req = Method "Req" [("x",TInt)] [("y",TInt)] [Requires (Predicate [] (Op2 (Var (Name "x")) Gt (Val (IntVal 0)))),Ensures (Predicate [] (Op2 (Var (Name "y")) Gt (Val (IntVal 0))))] (Block [Assign (Name "y") (Var (Name "x")),Empty])

ensures :: Method
ensures = Method "Ensures" [("x",TInt)] [("y",TInt)] [Ensures (Predicate [] (Op2 (Var (Name "y")) Eq (Var (Name "x"))))] (Block [Assign (Name "y") (Var (Name "x")),Empty])

ensures' :: Specification
ensures' = undefined

pred2 :: Predicate
pred2 = Predicate [] (Op2 (Var (Name "i")) Le (Op1 Len (Var (Name "a"))))

pred3 :: Predicate
pred3 = Predicate [] (Op2 (Var (Name "i")) Le (Op1 Len (Var (Name "b"))))

while2 :: Statement
while2 = While [pred2, pred3] exp3 block1

main :: IO ()
main = do
  -- putStrLn $ pretty exp1
  -- putStrLn $ pretty var1
  -- putStrLn $ pretty proj1
  -- putStrLn $ pretty decl1
  -- putStrLn $ pretty if1
  -- putStrLn $ pretty if2
  -- putStrLn $ pretty pred1
  putStrLn $ pretty loopToZeroCorrect
  -- putStrLn $ pretty while1
  -- putStrLn $ pretty wAbs
  -- putStrLn $ pretty wMinVal
  -- putStrLn $ pretty wMinIndex
  -- putStrLn $ pretty wMinMax
  -- putStrLn $ pretty wArraySpec
  -- putStrLn $ pretty intDiv
  -- putStrLn $ pretty manyBinops
  -- putStrLn $ pretty req
  -- putStrLn $ pretty ensures
  -- putStrLn $ pretty while2






