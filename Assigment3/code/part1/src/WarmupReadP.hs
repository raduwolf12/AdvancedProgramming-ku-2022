module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E ::= TE'| "-"TE'".
--   E'::= "+" TE'| "-"TE'|^.
--   T ::= num | "(" E ")" .


import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)


-- E ::= TE'| "-"TE'".
pExp :: Parser Exp
pExp = do e <- pTerm; pExp' e
       <|> do symbol "-";  e <- pTerm ; pExp' (Negate e)

-- E'::= "+" TE'| "-"TE'|^.
pExp' :: Exp -> Parser Exp -- argument is "accumulator"
pExp' e1 = do ao <- pAddOp; e2 <- pTerm; pExp' (ao e1 e2)
           <|> return e1 

--   T ::= num | "(" E ")" .
pTerm :: Parser Exp
pTerm = do n <- pNum; return $ Num n
        <|> do symbol "("; e <- pExp; symbol ")"; return e

lexeme :: Parser a -> Parser a
lexeme p = do skipSpaces ;a <- p ;skipSpaces ;return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

pNum :: Parser Int
pNum = lexeme $ do 
  ds <- many1 (satisfy isDigit)
  return $ read ds


pAddOp :: Parser (Exp -> Exp -> Exp)
pAddOp = do symbol "+"; return Add 
         <|> do symbol "-"; return (\x y-> Add x  (Negate y))

parseString :: String -> Either ParseError Exp
parseString s = if null (readP_to_S pExp s)
  then
    Left "Parse Failed."
    else do
      let tmp = [x | x <- readP_to_S pExp s, snd x == ""]
      ;if null tmp
        then
          Left  "Parse Failed."
          else
            Right (fst (head tmp))
            