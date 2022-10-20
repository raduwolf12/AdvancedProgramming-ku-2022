-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Control.Exception.Base
data MyException = LessThanZeroException
    deriving Show

instance Exception MyException

showExp :: Exp -> String
-- showExp (Cst n) = show n
showExp (Cst n) 
  | n < 0     = para (show n)
  | otherwise = show n
showExp (Add a b) = para (showExp a ++ "+" ++ showExp b)
showExp (Sub a b) = para (showExp a ++ "-" ++ showExp b)
showExp (Mul a b) = para (showExp a ++ "*" ++ showExp b)
showExp (Div a b) = para (showExp a ++ "`div`" ++ showExp b)
showExp (Pow a b) = para (showExp a ++ "^" ++ showExp b)
showExp _ = error "Error"

-- function that puts parentheses in front and back of a given string
para :: String -> String
para s = "(" ++ s ++ ")"


evalSimple :: Exp -> Integer
evalSimple (Cst n) = n
evalSimple (Add a b) = evalSimple a + evalSimple b
evalSimple (Sub a b) = evalSimple a - evalSimple b
evalSimple (Mul a b) = evalSimple a * evalSimple b
evalSimple (Div a b) = evalSimple a `div` evalSimple b
evalSimple (Pow a b)= do 
  if evalSimple b < 0
  then throw LessThanZeroException  
  else evalSimple a ^ evalSimple b
evalSimple _ = error "Error"


-- handler that catches arithmetic exceptions
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."

-- handler that catches a custom exception
handlerc :: MyException -> IO ()
handlerc LessThanZeroException  = putStrLn "Can't use pow!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r x  
  |(v==x) = Just n
  | otherwise =r x

evalFull :: Exp -> Env -> Integer
evalFull (Cst n) _ = n
evalFull (Add a b) env = evalFull a env + evalFull b env
evalFull (Sub a b) env = evalFull a env - evalFull b env
evalFull (Mul a b) env = evalFull a env * evalFull b env
evalFull (Div a b) env = evalFull a env `div` evalFull b env
evalFull (Pow a b) env
  |(evalFull b env >= 0 )= evalFull a env ^ evalFull b env
  |otherwise = throw LessThanZeroException

evalFull (If{test = a, yes = b, no=c} ) env 
  |((evalFull a env) > 0) = evalFull b env
  |((evalFull a env)==0) = evalFull c env
  |((evalFull a env) < 0) = evalFull b env
evalFull (Var a) env = case (env a) of
    Nothing -> throw LessThanZeroException
    Just v -> v
  
evalFull (Let {var = a, def = b, body = c} ) env = 
  let extendedEnv = extendEnv a (evalFull b env) env 
  in  evalFull c extendedEnv 

evalFull (Sum {var =a , from = b, to = c, body = d } ) env 
  |((evalFull b env) > (evalFull c env) ) = 0
  |((evalFull b env) == (evalFull c env) ) = evalFull (Let a b d) env
  |otherwise = 
    let aux = (evalFull c env )-1
     in  evalFull (Let a c d) env + evalFull (Sum a b (Cst aux) d) env 
evalFull _ _ = error "Error"

-- Extract the integer
getEither :: Either ArithError Integer -> Integer
getEither (Right x) = x
getEither _ = undefined


evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst n) _ = Right n 
evalErr (Add a b) env  = case evalErr a env of
                            Left az -> Left az 
                            Right az ->
                                case (evalErr b env) of
                                Left x -> Left x
                                Right x -> Right (az + x)


evalErr (Sub a b) env = case evalErr b env of
                            Left az -> Left az 
                            Right az ->
                                case (evalErr a env) of
                                Left x -> Left x
                                Right x -> Right (x - az)

evalErr (Mul a b) env = case evalErr a env of
                            Left az -> Left az 
                            Right az ->
                                case (evalErr b env) of
                                Left x -> Left x
                                Right x -> Right (x * az)

evalErr (Div a b) env = case evalErr b env of
                            Left az -> Left az 
                            Right az -> 
                              if az == 0 
                                then Left EDivZero 
                              else 
                                case (evalErr a env) of
                                Left x -> Left x
                                Right x -> Right (x `div` az)

evalErr (Pow a b) env = case evalErr b env of
                            Left az -> Left az 
                            Right az -> 
                              if az < 0 
                                then Left ENegPower 
                              else 
                                case evalErr a env of
                                Left x -> Left x
                                Right x -> Right (x ^ az)

evalErr (If{test = a, yes = b, no=c} ) env = case evalErr a env of
                            Left az -> Left az 
                            Right az -> 
                              if az == 0 
                                then  case (evalErr c env) of
                                Left x -> Left x
                                Right x -> Right x
                              else 
                                case (evalErr b env) of
                                Left x -> Left x
                                Right x -> Right x

evalErr (Var a ) env = case env a of 
  Nothing -> Left (EBadVar a)
  Just v -> Right v

 -- failed atempt of the Let function   
evalErr (Let {var = a, def = b, body = c} ) env = case evalErr b env   of
                            Left az -> Left az 
                            Right az -> 
                              let x = extendEnv a az env 
                              in evalErr c x

evalErr (Sum {var =a , from = b, to = c, body = d } ) env =
   case evalErr b env of
    Left az -> Left az
    Right az ->
      case (evalErr c env) of
        Left x -> Left x
        Right x ->
          if az > x 
            then Right 0
          else
            if az == x 
              then 
                let auxS =  Let a c d
                in evalErr auxS env
            else
              Right $ (getEither (evalErr d (extendEnv a az env))) + (getEither (evalErr (Sum a(Add b (Cst 1)) c d) env))
            

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
