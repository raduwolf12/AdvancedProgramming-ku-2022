-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))(Const (IntVal 2))]),
    SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "crash test negative" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal (-3)))(Const (IntVal (-2)))]),
    SExp (Var "hello")]
      @?= (["-5"], Just (EBadVar "hello")),
   testCase "test 1" $
    execute [SDef "x" (Const (IntVal 1)), SExp (Call "print" [(Var "x")])]
      @?= (["1"], Nothing),
   testCase "test 2" $
    execute [SDef "x" (Not (Const (IntVal 1))), SExp (Call "print" [(Var "x")])]
      @?= (["False"], Nothing),
   testCase "test 3" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))(Const (IntVal 2))]),
    SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "test 4" $
    execute [SExp (Call "print" [Oper Div (Const (IntVal 4)) (Const (IntVal 2))]),
    SExp (Call "print" [Oper Div (Const (IntVal 2)) (Const (IntVal 0))])]
      @?= (["2"], Just (EBadArg "error")),
   testCase "test 5" $
    execute [SExp (Call "print" [Oper Mod (Const (IntVal 3)) (Const (IntVal 2))]),
    SExp (Call "print" [Oper Mod (Const (IntVal 2)) (Const (IntVal 0))])]
      @?= (["1"], Just (EBadArg "error")),
   testCase "test 6" $
    execute [SExp (Call "print" [Oper Mod (Const TrueVal) (Const (IntVal 1))])]
      @?= ([], Just (EBadArg "error")),
   testCase "test 7" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 6)) (Const (IntVal 3))])]
      @?= (["3"], Nothing),
   testCase "test 8" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 3)) (Const (IntVal 4))])]
      @?= (["-1"], Nothing),
   testCase "test 9" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 3)) (Const (IntVal 3))])]
      @?= (["0"], Nothing),  
   testCase "test 10" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 3)) (Const (IntVal (-3)))])]
      @?= (["6"], Nothing), 
   testCase "test 11" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal (-3))) (Const (IntVal (-3)))])]
      @?= (["0"], Nothing),
   testCase "test 12" $ 
    execute [SExp (Call "print" [Oper Times (Const (IntVal 2)) (Const (IntVal 3))])]
      @?= (["6"], Nothing),
   testCase "test 13" $ 
    execute [SExp (Call "print" [Oper Times (Const (IntVal (-2))) (Const (IntVal 3))])]
      @?= (["-6"], Nothing),
   testCase "test 14" $ 
    execute [SExp (Call "print" [Oper Times (Const (IntVal (-4))) (Const (IntVal (-3)))])]
      @?= (["12"], Nothing),
   testCase "test 15" $ 
    execute [SExp (Call "print" [Oper Times (Const (IntVal 3)) (Const TrueVal)])]
      @?= ([], Just (EBadArg "error")),
   testCase "test 16" $ 
    execute [SExp (Call "print" [Oper Eq (Not (Const (IntVal 1))) (Const FalseVal),
     Oper Eq (Const (IntVal 1)) (Const FalseVal)])]
      @?= (["True False"], Nothing),
   testCase "test 17" $ 
    execute [SExp (Call "print" [Oper Eq (Not (Const (IntVal 1))) (Const (IntVal (-1))),
     Oper Eq (Const (IntVal 1)) (Const (IntVal 1))])]
      @?= (["False True"], Nothing),
   testCase "test 18" $
    execute [SExp (Call "print" [Oper Greater (Const (IntVal 1)) (Const (IntVal 3)),
     Oper Less (Const (IntVal 1)) (Const (IntVal 3))]),
     SExp (Call "print" [ Oper Less (Const (IntVal 0)) (Const TrueVal)])]
      @?= (["False True"], Just (EBadArg "error")),
   testCase "test 19" $
    execute [SExp (Call "print" [Oper In (Const (IntVal 2)) (Const (ListVal [IntVal 1, IntVal 2]))])]
      @?= (["True"], Nothing),
   testCase "test 20" $
    execute [SExp (Call "print" [Oper In (Const (IntVal 5)) (Const (ListVal [IntVal (-1), IntVal 2]))])]
      @?= (["False"], Nothing),
   testCase "test 21" $
    execute [SExp (Call "print" [Oper In (Const (IntVal 5)) (Const FalseVal)])]
      @?= ([], Just (EBadArg "error")),
   testCase "test 22" $
    execute [SExp (Call "a" [(Const TrueVal)])]
      @?= ([], Just (EBadFun "a")),
   testCase "test 23" $
    execute [SDef "x1" (Call "range" [(Const (IntVal 3))]),
    SDef "x2" (Call "range" [(Const (IntVal 1)), (Const (IntVal 0))]),
    SDef "x3" (Call "range" [(Const (IntVal 5)), (Const (IntVal 1)), (Const (IntVal (-2)))]),
    SExp (Call "print" [Var "x1", Var "x2", Var "x3"])]
      @?= (["[0, 1, 2] [] [5, 3]"], Nothing),
   testCase "test 24" $
    execute [SDef "x1" (Call "range" [(Const TrueVal)])]
      @?= ([], Just (EBadArg "error")),
   testCase "test 25" $
    execute [SDef "x3" (Call "range" [(Const (IntVal 5)), (Const (IntVal 1)), (Const (IntVal 0))])]
      @?= ([], Just (EBadArg "error")),
   testCase "test 26" $
    execute [SDef "x" (List [Oper Plus (Const (IntVal 10)) (Const (IntVal 25)), Const FalseVal]),
    SExp (Call "print" [Var "x"])]
      @?= (["[35, False]"], Nothing),
   testCase "test 27" $
    execute [SDef "x" (List [Const TrueVal, Const FalseVal]),
    SExp (Call "print" [Var "x"])]
      @?= (["[True, False]"], Nothing),
   testCase "test 28" $
    execute [SDef "x" (List [Const FalseVal, Const FalseVal]),
    SExp (Call "print" [Var "x"])]
      @?= (["[False, False]"], Nothing),
   testCase "test 29" $
    execute [SDef "x" (List [Const FalseVal, Const FalseVal, Const (StringVal "?")]),
    SExp (Call "print" [Var "x"])]
      @?= (["[False, False, ?]"], Nothing)]
   