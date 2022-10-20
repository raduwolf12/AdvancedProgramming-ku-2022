-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output, 
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp $ \_ -> (Right a, mempty)
  m >>= f = Comp $ \env -> case runComp m env of
      (Right a, _) ->  
        let (_, s1) = runComp m env
        in let (b, s2) = runComp (f a) env
        in (b, s1 <> s2)
      (Left error, s) -> (Left error, s)    

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp $ \_ -> (Left re, mempty)

look :: VName -> Comp Value
look name = Comp $ \env -> case lookup name env of 
  Just value-> (Right value, mempty) 
  Nothing -> (Left (EBadVar name), mempty)

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v x m = Comp (\env -> runComp m ((v, x):env))

output :: String -> Comp ()
output s = Comp $ \_ -> (Right (),[s])

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy x
  | x == NoneVal = False
  | x == FalseVal = False
  | x == IntVal 0 = False
  | x == StringVal "" = False
  | x == ListVal [] = False
  | otherwise = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
operate Div (IntVal v1) (IntVal v2)
  | v2 == 0 = Left "error"
  | otherwise = Right (IntVal (v1 `div` v2))
operate Mod (IntVal v1) (IntVal v2)
  | v2 == 0 = Left "error"
  | otherwise = Right (IntVal (v1 `mod` v2))
operate Eq v1 v2
  | v1 == v2 = Right TrueVal
  | otherwise = Right FalseVal
operate Less (IntVal v1) (IntVal v2)
  | v1 < v2 = Right TrueVal
  | otherwise = Right FalseVal
operate Greater (IntVal v1) (IntVal v2)
  | v1 > v2 = Right TrueVal
  | otherwise = Right FalseVal
operate In _ (ListVal []) = Right FalseVal
operate In v1 (ListVal (x:xs)) = 
    if operate Eq v1 x == Right TrueVal
      then Right TrueVal
    else
      operate In v1 (ListVal xs)
operate _ _ _ = Left "error"


listHelper :: [Value] -> String
listHelper [] = ""
listHelper (x:[]) = printHelper x
listHelper (x:xs) = printHelper x ++ ", " ++ listHelper xs

printHelper :: Value -> String
printHelper NoneVal = "None"
printHelper TrueVal = "True"
printHelper FalseVal = "False"
printHelper (IntVal c) = show c
printHelper (StringVal x1) = x1
printHelper (ListVal x) = "[" ++ listHelper x ++ "]"

printF :: [Value] -> String
printF [] = ""
printF (x:[]) = printHelper x
printF (x:xs) = printHelper x ++ " " ++ printF xs

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

getRight :: Either a b -> b
getRight (Right v) = v
getRight (Left _) = undefined

getLeft :: Either a b -> a
getLeft (Left s) = s
getLeft (Right _) = undefined

rangeFunction :: Value -> Value -> Value -> Either String [Value]
rangeFunction (IntVal x1) (IntVal x2) (IntVal x3)
  | x3 == 0 = Left "error"
  | x1 <= x2 && x3 < 0 = Right []
  | x1 >= x2 && x3 > 0 = Right []
  | otherwise = Right ((IntVal x1):(getRight (rangeFunction (IntVal (x1+x3)) (IntVal x2) (IntVal x3))))
rangeFunction _ _ _ = Left "error"

apply :: FName -> [Value] -> Comp Value
apply "range" [e1, e2, e3] =
  let tmp = rangeFunction e1 e2 e3 in
    if isRight tmp
      then wrapComp (ListVal (getRight tmp))
    else
      abort (EBadArg (getLeft tmp))
apply "range" [e1, e2] = apply "range" [e1, e2, IntVal 1]
apply "range" [e2] = apply "range" [IntVal 0, e2, IntVal 1]
apply "range" _ = abort (EBadArg "error")
apply "print" x = 
  do 
  {
    output (printF x)
    ;return NoneVal
  }

apply a _ = abort (EBadFun a)

wrapComp :: Value -> Comp Value
wrapComp x = withBinding "_" x (look "_")

extractList :: Value -> [Value]
extractList (ListVal x) = x
extractList _ = []

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const x) = wrapComp x
eval (Var v) = look v
eval (Oper op e1 e2) =
  do
  {
    x1 <- eval e1
    ;x2 <- eval e2
    ;let tmp = operate op x1 x2 in
      if isRight tmp
        then wrapComp (getRight tmp)
      else
        abort (EBadArg (getLeft tmp))
  }

eval (Not e) =
  do
  {
    tmp <- eval e
    ;
    if truthy tmp
      then wrapComp FalseVal
    else
      wrapComp TrueVal
  }

eval (Call f []) = apply f []
eval (Call f e) = 
  do 
  {
    x <- eval (List e)
    ;apply f (extractList x)
  }

eval (List []) = wrapComp (ListVal [])
eval (List (e:es)) = 
  do 
  {
    x <- eval e
    ;xs <- eval (List es)
    ;wrapComp (ListVal (x:(extractList xs)))
  }

eval (Compr _ []) = wrapComp NoneVal
eval (Compr _ _) = undefined
-- eval (Compr e [CCFor name ex]) = undefined
-- eval (Compr e [CCIf ex]) = undefined
-- eval (Compr e cc) = case cc of 
-- eval (Compr e ((name ex):es)) = undefined
-- eval (Compr e (ex:es))
  -- do
  -- {
  --   x <- eval e
  --   ;xs <- eval (CClause es)
  --   -- ;wrapComp (ListVal (x:(extractList xs)))
  -- }
-- eval (Compr e cc) = case cc of 
  -- [CCFor name ex]  ->   
  --   do
  --     {
  --       x <- eval ex
  --       ;xs <- eval (CClause e)
  --       ;wrapComp (ListVal (x:(extractList xs)))
  --       }
  -- [CCIf ex] -> ex
  -- (cc':xs) ->  xs
-- eval (Compr e [CCFor name ex]) = 
--   do
--   {
--     x <- eval ex
--     ;xs <- eval (List es)
--     ;wrapComp (ListVal (x:(extractList xs)))
--   }


exec :: Program -> Comp ()
exec [] = return mempty

exec ((SDef v e):xs) =
  do {
    x <- eval e
    ;withBinding v x (exec xs)
  }
exec ((SExp e):xs) =
  do {
    eval e
    ;exec xs
  }

execute :: Program -> ([String], Maybe RunError)
execute x = 
  if isRight (fst (runComp (exec x) []))
    then (snd (runComp (exec x) []), Nothing)
  else (snd (runComp (exec x) []), Just (getLeft (fst (runComp (exec x) []))))
