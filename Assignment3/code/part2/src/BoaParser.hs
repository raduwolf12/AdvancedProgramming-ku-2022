-- Skeleton file for Boa Parser.
module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import Control.Monad
-- add any other other imports you need

type Parser a = ReadP a
type ParseError = String -- you may replace this

reserveWords :: [String]
reserveWords = ["None", "True", "False", "for", "if", "in", "not"]

--works like skipSpaces but also skips comments
skip :: Parser ()
skip = do
  skipSpaces;
  s <- look
  when (not (null s) && head s == '#') $ do
    manyTill get (satisfy (== '\n') <|> do eof; return 'a')
    skip

--Like skip, however there should be at the least one whitespace or the following expression starts with a bracket
skipWB :: Parser ()
skipWB = do
  s <- look
  if not (null s) && (head s == '(' || head s == '[')
    then return ()
    else if head s == '#'
      then skip
      else do
        munch1 isSpace
        skip

--parses an identifier
pIdent :: Parser String
pIdent = do
  s <- look
  if any (\str -> take (length str) s == str && (null (drop (length str) s) || not(stringChar (s !! max 0 (length str))))) reserveWords
    then pfail
    else do
      c <- satisfy (\c -> isAlpha c || c=='_')
      cs <- munch stringChar
      return (c:cs)
  where
    stringChar :: Char -> Bool
    stringChar c = isAlphaNum c || c=='_'

lexeme :: Parser a -> Parser a
lexeme p = do skipSpaces ;a <- p ;skipSpaces ;return a


--parses a numeric constant
pNumConst :: ReadP Exp
pNumConst = do
  x <- option ' ' (char '-')
  ;xs <- munch1 isDigit
  ;if (head xs == '0') && (length xs > 1)
    then fail "Num Error"
  else return (Const (IntVal (read (x:xs)::Int)))

--parses a string constant
pStringConst :: Parser Exp
pStringConst = do
  lexeme ( string "\'")
  str <- pStr ""
  skip;
  return (Const (StringVal str))

pStr :: String -> Parser String
pStr s = do lexeme ( string "\\"); lexeme ( string "n"); pStr $ s ++ "\n"
  <|> do lexeme ( string "\\"); lexeme ( string "\'"); pStr $ s ++ "'"
  <|> do lexeme ( string "\\"); lexeme ( string "\n"); pStr s
  <|> do lexeme ( string "\\"); lexeme ( string "\\"); pStr $ s ++ "\\"
  <|> do lexeme ( string "\'"); return s
  <|> do c <- satisfy (\c -> c /= '\\' && c /= '\'' && isPrint c); pStr $ s ++ [c]

pStmts :: Parser [Stmt]
pStmts = do s <- pStmt; ss <- pStmt'; skip; return $ s:ss

pStmt' :: Parser [Stmt]
pStmt' = do lexeme (string ";"); skip; pStmts
  <|> return []


pStmt :: Parser Stmt
pStmt = do i <- pIdent; skip; lexeme ( string "="); skip; e<-pExp; return $ SDef i e
  <|> do e <- pExp; return $ SExp e


pNot :: ReadP Exp
pNot = do lexeme (string "not"); skipWB; e <- pExp; return (Not e)

pExp :: ReadP Exp
pExp = pNot <++ pExpOrd

pExpOrd :: Parser Exp
pExpOrd = (do
  e1 <- pExpAdd
  ;_ <- lexeme (string "==")
  ;e2 <- pExpAdd
  ;return (Oper Eq e1 e2))
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string "!=")
    ;e2 <- pExpAdd
    ;return (Not (Oper Eq e1 e2))
    )
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string "<")
    ;e2 <- pExpAdd
    ;return (Oper Less e1 e2)
    )
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string ">")
    ;e2 <- pExpAdd
    ;return (Oper Greater e1 e2)
    )
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string "<=")
    ;e2 <- pExpAdd
    ;return (Not (Oper Greater e1 e2))
    )
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string ">=")
    ;e2 <- pExpAdd
    ;return (Not (Oper Less e1 e2))
    )
  <++ (do 
    e1 <- pExpAdd
    ;_ <- lexeme (string "in ")
    ;e2 <- pExpAdd
    ;return (Oper In e1 e2))
  <++ (do
    e1 <- pExpAdd
    ;_ <- lexeme (string "not ")
    ;_ <- lexeme (string "in ")
    ;e2 <- pExpAdd
    ;return (Not (Oper In e1 e2)))
  <++ do skip; pExpAdd

pExpAdd :: Parser Exp
pExpAdd = do e <- pExpMul; pExpAdd' e


pExpAdd' :: Exp -> Parser Exp
pExpAdd' e = do lexeme ( string "+"); skip; f <- pExpMul; pExpAdd' (Oper Plus e f)
  <|> do lexeme ( string "-"); skip; f <- pExpMul; pExpAdd' (Oper Minus e f)
  <|> return e


pExpMul :: Parser Exp
pExpMul = do e <- pExpTerm; pExpMul' e

pExpMul' :: Exp -> Parser Exp
pExpMul' e = do lexeme ( string "*"); skip; f <- pExpTerm; pExpMul' (Oper Times e f)
  <|> do lexeme ( string "%"); skip; f <- pExpTerm; pExpMul' (Oper Mod e f)
  <|> do lexeme ( string "//"); skip; f <- pExpTerm; pExpMul' (Oper Div e f)
  <|> return e

pNone :: ReadP Exp
pNone = do lexeme (string "None"); skip; return (Const NoneVal)

pTrue :: ReadP Exp
pTrue = do lexeme (string "True"); skip; return (Const TrueVal)

pFalse :: ReadP Exp
pFalse = do lexeme (string "False"); skip; return (Const FalseVal)


pPharantesis :: ReadP Exp
pPharantesis = do lexeme ( string "("); skip; e <- pExp; lexeme ( string ")"); skip; return e

pBrackets :: ReadP Exp
pBrackets = do lexeme ( string "["); skip; e <- pExpB; lexeme ( string "]"); skip; return e


pExpTerm :: Parser Exp
pExpTerm = pNumConst
  <|> pStringConst
  <|> pNone
  <|> pTrue
  <|> pFalse 
  <|> do i <- pIdent; skip; pExpI i
  <|> pPharantesis
  <|> pBrackets

pExpI :: String -> Parser Exp
pExpI s = do lexeme ( string "("); skip; e <- pExpz s; lexeme ( string ")"); skip; return e
  <|> return (Var s)

pExpz :: String -> Parser Exp
pExpz s = do e <- pExp; pExpzs s [e]
  <|> return (Call s [])

pExpzs :: String -> [Exp] -> Parser Exp
pExpzs s es = do lexeme ( string ","); skip; e <- pExp; pExpzs s (es++[e])
  <|> return (Call s es)

pExpB :: Parser Exp
pExpB = do e <- pExp; pExpB' e
  <|> return (List [])

pExpB' :: Exp -> Parser Exp
pExpB' e = pExps [e]
  <|> do f <- pFor; pClausez e [f]

pExps :: [Exp] -> Parser Exp
pExps es = do lexeme ( string ","); skip; e <- pExp; pExps (es ++ [e])
  <|> return (List es)

pFor :: Parser CClause
pFor = do
  string "for"
  skip
  i <- pIdent
  skipWB
  string "in"
  skipWB
  e <- pExp
  skip;
  return (CCFor i e)

pIf :: Parser CClause
pIf = do
  lexeme ( string "if")
  skipWB
  e <- pExp
  skip;
  return (CCIf e)

pClausez :: Exp -> [CClause] -> Parser Exp
pClausez e cs = do f <- pFor; pClausez e (cs++[f])
  <|> do i <- pIf; pClausez e (cs++[i])
  <|> return (Compr e cs)

parseString :: String -> Either ParseError Program
parseString s = if null (readP_to_S pStmts s)
  then
    Left "cannot parse"
    else do
      let tmp = [x | x <- readP_to_S pStmts s, snd x == ""]
      ;if null tmp
        then
          Left  "cannot parse"
          else
            Right (fst (head tmp))