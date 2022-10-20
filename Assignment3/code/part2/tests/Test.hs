-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests


tests = testGroup "Tests"
  [
  testGroup "Minimal tests" [
    testCase "simple success" $    parseString "2 + two" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
    testCase "simple failure" $ 
      -- avoid "expecting" very specific parse-error messages
      case parseString "wow!" of
        Left e -> return ()  -- any message is OK
        Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ],
  testGroup "Tests" [
    testCase "Int value" $ parseString "1" @?= Right [SExp (Const (IntVal 1))],
    testCase "Big Int value" $ parseString "122222222222222" @?= Right [SExp (Const (IntVal 122222222222222))],
    testCase "String value" $ parseString "a" @?= Right [SExp (Var "a")],
    testCase "Long String value" $ parseString "qwertyuiopasdfghjklzxcvbnm" @?= Right [SExp (Var "qwertyuiopasdfghjklzxcvbnm")],
    testCase "String value plus Int value" $ parseString "qwertyuiopasdfghjklzxcvbnm123_231124+2" @?= Right [SExp (Oper Plus (Var "qwertyuiopasdfghjklzxcvbnm123_231124") (Const (IntVal 2)))],
    testCase "String value with operation with comments in between" $ parseString "ab+2//3#test\n#test\n%3" @?= Right [SExp (Oper Plus (Var "ab") (Oper Mod (Oper Div (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 3))))],
    testCase "Empty string" $ parseString " " @?= (Left "cannot parse"),
    testCase "Special character string" $ parseString "'!@'" @?= Right [SExp (Const (StringVal "!@"))],
 

    testCase "Num, Stmts ;" $ parseString "-1234; 1" @?= Right [SExp (Const (IntVal (-1234))),SExp (Const (IntVal 1))],
    testCase "String" $ parseString "'\\'asf124\\\n\\n  f'" @?= Right [SExp (Const (StringVal "'asf124\n  f"))],
    testCase "String failure" $
      case parseString "'\'123" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
    testCase "Exp indent ()" $ parseString "print (123)" @?= Right [SExp (Call "print" [Const (IntVal 123)])],
    testCase "CClause" $ parseString "[a for k in 1 if c]" @?= Right [SExp (Compr (Var "a") [CCFor "k" (Const (IntVal 1)),CCIf (Var "c")])],
    testCase "Stmt indent" $ parseString "a = 1" @?= Right [SDef "a" (Const (IntVal 1))],
    testCase "Call 0 args" $ parseString "f()" @?= Right [SExp (Call "f" [])],
    testCase "Call 1 args" $ parseString "g(True)" @?= Right [SExp (Call "g" [Const TrueVal])],
    testCase "Call 2 args" $ parseString "h(True,k())" @?= Right [SExp (Call "h" [Const TrueVal,Call "k" []])],
    testCase "Strange ranges" $ parseString "[range(), range(1,2,3,4), range(None)]" @?= Right [SExp (List [Call "range" [],Call "range" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3),Const (IntVal 4)],Call "range" [Const NoneVal]])],
    testCase "Def" $ parseString "x=y " @?= Right [SDef "x" (Var "y")],
    testCase "Seq" $ parseString "x;y=z;u;v=x " @?= Right [SExp (Var "x"),SDef "y" (Var "z"),SExp (Var "u"),SDef "v" (Var "x")],
    testCase "Eq" $ parseString "x=y==z" @?= Right [SDef "x" (Oper Eq (Var "y") (Var "z"))],
    testCase "Seq1" $ parseString "x;x in [1,2,3]" @?= Right [SExp (Var "x"),SExp (Oper In (Var "x") (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)]))],
    testCase "Seq2 " $ parseString "[1,2,3];[4,5,6];[7,8,9]" @?= Right [SExp (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)]),SExp (List [Const (IntVal 4),Const (IntVal 5),Const (IntVal 6)]),SExp (List [Const (IntVal 7),Const (IntVal 8),Const (IntVal 9)])]
    ],
  testGroup "Test expresions" [
    testCase "None" $ parseString "None" @?= Right [SExp (Const NoneVal)],
    testCase "True" $ parseString "True" @?= Right [SExp (Const TrueVal)],
    testCase "False" $ parseString "False" @?= Right [SExp (Const FalseVal)],
    testCase "Exp Not" $ parseString "not a" @?= Right [SExp (Not (Var "a"))],
    testCase "Exp no space Not -> Var" $ parseString "nota" @?= Right [SExp (Var "nota")],
    testCase "Comprehension for in in" $ parseString "[x for y in z in u]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper In (Var "z") (Var "u"))])],
    testCase "Comprehension " $ parseString "[x for y in z if u for a in []]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Var "z"),CCIf (Var "u"),CCFor "a" (List [])])],
    testCase "comprehension for ynot in " $ parseString "[x for ynot in z]" @?= Right [SExp (Compr (Var "x") [CCFor "ynot" (Var "z")])]

  ],
  testGroup "Test operator" [
    testCase "Oper +" $ parseString "1 + 2 " @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper +" $ parseString "1 + -2 " @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal (-2))))],
    testCase "Oper -" $ parseString "1 - 2 " @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper -" $ parseString "1 - -2 " @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal (-2))))],
    testCase "Oper *" $ parseString "1*2 " @?= Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper *" $ parseString "1*-2 " @?= Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal (-2))))],
    testCase "Oper //" $ parseString "1//2 " @?= Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper //" $ parseString "1//-2 " @?= Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal (-2))))],
    testCase "Oper %" $ parseString "1%2 " @?= Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper %" $ parseString "1%-2 " @?= Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal (-2))))],
    testCase "Oper + - * // %" $ parseString "[x+y,x-y,x*y,x//y,x%y] " @?= Right [SExp (List [Oper Plus (Var "x") (Var "y"),Oper Minus (Var "x") (Var "y"),Oper Times (Var "x") (Var "y"),Oper Div (Var "x") (Var "y"),Oper Mod (Var "x") (Var "y")])],


    testCase "Oper + * %" $ parseString "1 + 2 * 3 % 4" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Mod (Oper Times (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 4))))],
    testCase "Oper - // ()" $ parseString "1 - 2 // (1+1)" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))))],
    testCase "Oper not in, []" $ parseString "a not in [1, 2]" @?= Right [SExp (Not (Oper In (Var "a") (List [Const (IntVal 1),Const (IntVal 2)])))],
   
   
    testCase "Oper <" $ parseString "1<2 " @?= Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper ==" $ parseString "1==2 " @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper >" $ parseString "1==2 " @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Oper >=" $ parseString "a >= 6" @?= Right [SExp (Not (Oper Less (Var "a") (Const (IntVal 6))))],
    testCase "Oper <=" $ parseString "1<=2 " @?= Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 2))))],
    testCase "Oper < and >" $ parseString "1 < a > 3" @?= (Left "cannot parse"),  
    testCase "Oper >= and ==" $
    case parseString "a >= 6 == 6" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
    testCase "Oper == < > in" $ parseString "[x==y,x<y,x>y,x in y] " @?= Right [SExp (List [Oper Eq (Var "x") (Var "y"),Oper Less (Var "x") (Var "y"),Oper Greater (Var "x") (Var "y"),Oper In (Var "x") (Var "y")])],
    testCase "Oper  != <= >= not in" $ parseString "[x!=y,x>=y,x<=y,x not in y] " @?= Right [SExp (List [Not (Oper Eq (Var "x") (Var "y")),Not (Oper Less (Var "x") (Var "y")),Not (Oper Greater (Var "x") (Var "y")),Not (Oper In (Var "x") (Var "y"))])]

  ],
    testGroup "Test operator associativity/precedence" [
      testCase "associativity of add" $ parseString "1+2+3" @?= (Right [SExp (Oper Plus (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of mul" $ parseString "1*2*3" @?= (Right [SExp (Oper Times (Oper Times (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of div/mod" $ parseString "1//2%3" @?= (Right [SExp (Oper Mod (Oper Div (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of minus" $ parseString "1-2-3" @?= (Right [SExp (Oper Minus (Oper Minus (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "precedence test" $ parseString "not 1+2*3-(not 4//5+6)" @?= (Right[SExp (Not(Oper Minus (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3)))) (Not (Oper Plus (Oper Div (Const (IntVal 4)) (Const (IntVal 5))) (Const (IntVal 6))))))]),
      testCase "no associativity of <,>,==" $ parseString "1 < 2 > 3 == 4" @?= (Left "cannot parse"),
      testCase "no associativity of <=,>=,==" $ parseString "1 <= 2 >= 3 == 4" @?= (Left "cannot parse")
      ],
    testGroup "Tests comments and whitespaces" [
      testCase "missing whitespace between keywords" $ parseString "a notin b" @?= (Left "cannot parse"),
      testCase "no whitespace, but bracket" $ parseString "not(False)" @?= (Right [SExp (Not (Const FalseVal))]),
      testCase "Comments" $ parseString "True#test\n#test1\t\n;False" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "Comments at eof" $ parseString "True#test" @?= (Right [SExp (Const TrueVal)]),
      testCase "empty comment" $ parseString "True;#\nFalse" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "skipping newlines" $ parseString "True\n\n;\nFalse" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "skipping tabs" $ parseString "tab\t\t\t;False" @?= (Right [SExp (Var "tab"),SExp (Const FalseVal)]),
      testCase "comment as one whitespace" $ parseString "not#comment\ncool" @?= (Right [SExp (Not (Var "cool"))]),
      testCase "Tabs and newlines" $ parseString "\t\n [\t \n] \n\t" @?= Right [SExp (List [])],
      testCase "Interspersed whitespaces" $ parseString " x = ( 2 ) ; print ( None == [ y , z ] , not u ) " @?= Right [SDef "x" (Const (IntVal 2)),SExp (Call "print" [Oper Eq (Const NoneVal) (List [Var "y",Var "z"]),Not (Var "u")])],
      testCase "No whitespaces" $ parseString "[(x)not\tin(not(y)),[(x)for\ty\tin[z]if(u)]]" @?= Right [SExp (List [Not (Oper In (Var "x") (Not (Var "y"))),Compr (Var "x") [CCFor "y" (List [Var "z"]),CCIf (Var "u")]])],
      testCase "Comment as separator" $ parseString "not#foo\nx" @?= Right [SExp (Not (Var "x"))],
      testCase "Only comments" $ parseString "#com#com1#com2#com3" @?= (Left "cannot parse"),    
      testCase "Large whitespaces" $ parseString "                                                      x                                                           " @?= Right [SExp (Var "x")],
      testCase "Many comments" $ parseString "#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \nx#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \n#  \n" @?= Right [SExp (Var "x")]  
      ],
    testGroup "Tests of ident" [
      testCase "keyword as identifier" $ parseString "in = out" @?= (Left "cannot parse"),
      testCase "keyword in identifier" $ parseString "inside = outside" @?= (Right [SDef "inside" (Var "outside")]),
      testCase "starting with _" $ parseString "_underscore = UP" @?= (Right [SDef "_underscore" (Var "UP")]),
      testCase "starting with number" $ parseString "112 = alarm" @?= (Left "cannot parse"),
      testCase "numbers, underscore and letters" $ parseString "_abc9 < Xyzw100_" @?= (Right [SExp (Oper Less (Var "_abc9") (Var "Xyzw100_"))])
      ],
    testGroup "Tests of numConst" [
      testCase "minus zero '-0'" $ parseString "-0" @?= (Right [SExp (Const (IntVal 0))]),
      testCase "plus in front of number '+0'" $ parseString "+0" @?= (Left "cannot parse"),
      testCase "wrong number format '1.0'" $ parseString "1.0" @?= (Left "cannot parse"),
      testCase "space between minus and number '- 4'" $ parseString "- 4" @?= (Left "cannot parse"),
      testCase "starting with zeros '007'" $ parseString "007" @?= (Left "cannot parse")
      ],
    testGroup "Tests of stringConst" [
      testCase "'basic string'" $ parseString "'basic string'" @?= (Right [SExp (Const (StringVal "basic string"))]),
      testCase "'a\\nb'" $ parseString "'a\\\nb'" @?= (Right [SExp (Const (StringVal "ab"))]),
      testCase "'\\\\'" $ parseString "'\\\\'" @?= (Right [SExp (Const (StringVal "\\"))]),
      testCase "'a#bc'" $ parseString "'a#bc'" @?= (Right [SExp (Const (StringVal "a#bc"))]),
      testCase "'\\''" $ parseString "'\\''" @?= (Right [SExp (Const (StringVal "'"))]),
      testCase "'\\x'" $ parseString "'\\x'" @?= (Left "cannot parse"),
      testCase "Simple escapes" $ parseString "'a\\'b\\\\c\\nd'" @?= Right [SExp (Const (StringVal "a'b\\c\nd"))],    
      testCase "Escaped newlines" $ parseString "'a\\\n b\\n\\\nc\\\n\\nd'" @?= Right [SExp (Const (StringVal "a b\nc\nd"))]  
      ],
    testGroup "Test parenthesis" [
      testCase "brackets []" $ parseString "[1]" @?= (Right [SExp (List [Const (IntVal 1)])]),
      testCase "brackets ()" $ parseString "(1)" @?= (Right [SExp (Const (IntVal 1))]),
      testCase "deep brackets []" $ parseString "[[[[[[[[[[[x]]]]]]]]]]]" @?= (Right [SExp (List [List [List [List [List [List [List [List [List [List [List [Var "x"]]]]]]]]]]])]),
      testCase "very deep brackets []" $ parseString "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[x]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]" @?= (Right [SExp (List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [Var "x"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]])]),
      testCase "deep brackets ()" $ parseString "(((((((((((x)))))))))))" @?= (Right [SExp (Var "x")]),
      testCase "very deep brackets ()" $ parseString "(((((((((((((((((((((((((((((((((x)))))))))))))))))))))))))))))))))" @?= (Right [SExp (Var "x")])
      ]
    ]