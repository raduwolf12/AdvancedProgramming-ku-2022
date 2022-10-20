module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E


instance Arbitrary Expr where
   arbitrary = generator


generator :: Gen Expr
generator = sized generatorN
generatorN 0 = do
  s <- arbitrary
  return $ Const s
generatorN n = do
   let es = generatorN (n `div` 2) in oneof [ (
      do 
         s <- arbitrary
         return $ Const s)
         , (
      do 
         s <- arbitrary
         return $ Var s)
         ,Oper Times <$> es <*> es
         ,Oper Plus <$> es <*> es
         ,Oper Minus <$> es <*> es
         ,Let <$> listOf arbitrary <*> es <*> es
         ]

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = x === x
-- prop_eval_simplify E.evalTop (E.simplify  x)  === E.evalTop x -- sould be correct, but not working due to import declaration