module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   <<< E ::= T E' | "-" T E' .
--       E' ::= € | "+" T E' | "-" T E' .
--       T ::= num | "(" E ")" .
--  >>>

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

pNum :: Parser Int
pNum = lexeme $ do ds <- many1 (satisfy isDigit); return $ read ds

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser()
symbol s = lexeme $ do string s; return ()

pE :: Parser Exp
pE = do exp <- pT; pE' exp;
     <|> do whitespace; char '-'; exp <- pT; exp2 <- pE' $ Negate exp;  return exp2;

pE' :: Exp -> Parser Exp
pE' exp = do whitespace; symbol "+"; exp2 <- pT; pE' (Add exp exp2);
          <|> do whitespace; symbol "-"; exp2 <- pT; pE' (Add exp (Negate exp2))
          <|> return exp;

pT :: Parser Exp
pT = do whitespace; n <- pNum; return $ Num n
     <|> do whitespace; symbol "("; whitespace; exp <- pE; whitespace; symbol ")"; whitespace; return exp

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do whitespace; a <- pE; eof; return a) s of 
  [] -> Left "cannot parse"
  x -> case last x of 
    (exp,_) -> Right exp
    _ -> Left "error !!!!"

