-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21,
 test22, test23,test24,test25,test26,test27,test28,test29,test30,test31,test32] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalSimple (Mul (Cst 3) (Cst 2)) == 6)
  test3 = ("test3", evalSimple (Sub (Cst 4) (Cst 2)) == 2)
  test4 = ("test4", evalSimple (Div (Cst 6) (Cst 2)) == 3)
  test5 = ("test5", evalSimple (Mul (Add (Cst 2) (Cst 3)) (Cst 4)) == 20) 
  test6 = ("test6", evalSimple (Div (Cst (-10)) (Cst (-2))) == 5)
  test7 = ("test7", evalSimple (Pow (Cst 3) (Cst 3)) == 27)
  test8 = ("test8", evalSimple (Pow (Cst (-3)) (Cst 3)) == -27)
  test9 = ("test9", evalSimple (Mul (Cst 1234567890) (Cst 1234567890)) == 1524157875019052100)
  test10 = ("test10", evalSimple (Pow (Cst 0) (Cst 1)) == 0)
  
  test11 = ("test11", evalFull (Add (Cst 42) (Cst 1)) initEnv == 43)
  test12 = ("test12", evalFull (Sum "x" (Cst 1) (Cst 5) (Pow (Var "x") (Cst 2))) initEnv == 55)
  test13 = ("test13", evalFull (If (Cst 1) (Cst 4) (Cst 5)) initEnv == 4)
  test14 = ("test14", evalFull (Sub (Cst 42) (Cst 1)) initEnv == 41)
  test15 = ("test15", evalFull (Div (Cst 42) (Cst 2)) initEnv == 21)
  test16 = ("test16", evalFull (Pow (Cst 5) (Cst 2)) initEnv == 25)
  test17 = ("test17", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test20 = ("test20", evalFull (If (Pow (Cst 20)(Cst 20)) (Cst 4) (Cst 5)) initEnv == 4)
  test21 = ("test21", evalFull (Mul (Add (Cst 2) (Cst 3)) (Cst 4)) initEnv == 20)
  test22 = ("test22", evalFull (If (Sub (Cst 20)(Cst 20)) (Cst 4) (Cst 5)) initEnv == 5)
  
  test18 = ("test18", showExp (Add (Cst 2) (Cst 2)) == "(2+2)")   
  test19 = ("test19", showExp (Mul (Cst 2) (Cst 2)) == "(2*2)") 

  test23 = ("test23", evalErr (Add (Cst 42) (Cst 1)) initEnv == Right (43))
  test24 = ("test24", evalErr (If (Cst 1) (Cst 4) (Cst 5)) initEnv == Right 4)
  test25 = ("test25", evalErr (Sub (Cst 42) (Cst 1)) initEnv == Right 41)
  test26 = ("test26", evalErr (Div (Cst 42) (Cst 2)) initEnv == Right 21)
  test27 = ("test27", evalErr (Pow (Cst 5) (Cst 2)) initEnv == Right 25)
  test28 = ("test28", evalErr (If (Pow (Cst 20)(Cst 20)) (Cst 4) (Cst 5)) initEnv == Right 4 )
  test29 = ("test29", evalErr (Mul (Add (Cst 2) (Cst 3)) (Cst 4)) initEnv == Right 20)
  test30 = ("test30", evalErr (If (Sub (Cst 20)(Cst 20)) (Cst 4) (Cst 5)) initEnv == Right 5)

  test31 = ("test31", evalErr (Div (Cst 42) (Cst 0)) initEnv == Left EDivZero)
  test32 = ("test32", evalErr (Pow (Cst 42) (Cst (-1))) initEnv == Left ENegPower)


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure