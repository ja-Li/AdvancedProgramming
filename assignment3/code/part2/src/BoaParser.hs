
-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP as RP
import Data.Char
import Control.Applicative

-- add any other other imports you need

newtype ParseError = ParseError String deriving (Eq, Show)


whitespace :: ReadP Char
whitespace = do skipSpaces; skipMany (do char '#'; RP.many $ satisfy isAscii; char '\n'); return '\0'
             <++ do skipMany (do char '#'; RP.many $ satisfy isAscii; RP.eof); return '\0'
             <++ do RP.many(satisfy isSpace); return '\0'
-- type ParseError = String -- you may replace this
-- type Var = String

-- comments :: ReadP ()
-- comments = do skipMany (do char '#'; str <- munch(\x-> x /= '\n');char '\n';return str);
--              <|> do skipMany (do char '#'; str <- munch(\x-> x /= '\n'); RP.eof;return str);

comments :: ReadP a -> ReadP a
comments p = do whitespace; a <- p ; whitespace;return a

lexeme :: ReadP a -> ReadP a
lexeme p = do skipSpaces ; a <- p ; skipSpaces ;return a

-- skipcomments::ReadP a -> ReadP a
-- skipcomments p = do skipSpaces
--                     skipMany (do char '#'; str <- munch(\x-> x /= '\n');char '\n')
--                             --   <|> do char '#'; str <- munch(\x-> x /= '\n'); RP.eof;return str)
--                     a<-p
--                     skipMany (do char '#'; str <- munch(\x-> x /= '\n');char '\n';return str)
--                     skipMany (do char '#'; str <- munch(\x-> x /= '\n'); RP.eof;return str)
--                     skipSpaces
--                     return a 

pProgram :: ReadP Program
pProgram = pStmts

pStmts :: ReadP [Stmt]
pStmts = do st <- lexeme $ comments  pStmt; lexeme $ string ";"; sts <- lexeme $ comments pStmts; return (st:sts)
         <++ do st <- lexeme $ comments pStmt; return [st]

-- ignoreComments:: ReadP ()
-- ignoreComments = do whitespace; skipMany (do string "#"; str <- munch(\x-> x /= '\n'); string "\n";return str)

pStmt :: ReadP Stmt
pStmt = do i <- lexeme $ comments pIdent; lexeme $ string "="; exp <- lexeme $ comments pExpr; return (SDef i exp)
        <++ do exp <- lexeme $ comments pExpr; return (SExp exp)

reservedWords :: [String]
reservedWords = ["None", "True", "False", "for", "if", "in", "not"]

pIdent :: ReadP String
pIdent = lexeme $ do
    x <- satisfy (\capital -> isAlpha capital || capital == '_')
    xs <- RP.munch (\n -> isAlpha n || isDigit n || n == '_' )
    let i = x:xs
    if i `notElem` reservedWords then return i
        else fail "Variable can't be a reserved word."

pNumConst :: ReadP Exp
pNumConst = lexeme (do
    char '-'
    xs <- munch1 isDigit
    if head xs == '0' && length xs > 1 then fail "Wrong num!"
        else return $ BoaAST.Const (IntVal (read ('-':xs)))
        )
    <|> lexeme (do
        xs <- munch1 isDigit
        if head xs /= '0' || head xs == '0' && length xs == 1 then return $ BoaAST.Const (IntVal (read xs))
            else fail "Wrong num!")

pExpr :: ReadP Exp
pExpr = do lexeme $ string "not";char '\0'; skipSpaces; exp <- lexeme pExpr;return (Not exp);
        <++ pExpr'

pExpr' :: ReadP Exp
pExpr' = do t <- pAddOper'; lexeme $ string "=="; e' <- pAddOper'; return (Oper Eq t e')
          <++ do t <- pAddOper'; lexeme $ string "!="; e' <- pAddOper'; return $ Not (Oper Eq t e');
          <++ do t <- pAddOper'; lexeme $ string ">"; e' <- pAddOper'; return (Oper Greater t e')
          <++ do t <- pAddOper'; lexeme $ string "<"; e' <- pAddOper'; return (Oper Less t e')
          <++ do t <- pAddOper'; lexeme $ string ">="; e' <- pAddOper'; return $ Not (Oper Less t e');
          <++ do t <- pAddOper'; lexeme $ string "<="; e' <- pAddOper'; return $ Not (Oper Greater t e');
          <++ do t <- pAddOper'; lexeme $ string "in "; e' <- pAddOper'; return (Oper In t e')
          <++ do t <- pAddOper'; lexeme $ string "not "; lexeme $ string "in "; e' <- pAddOper'; return $ Not (Oper In t e');
          <++ pAddOper'

pAddOper' :: ReadP Exp
pAddOper' = do t <- pMultOper'; pAddOper t;
            <++ pMultOper'

pAddOper :: Exp -> ReadP Exp
pAddOper t = do lexeme $ string "+"; e' <- pMultOper'; pAddOper (Oper Plus t e')
             <++ do lexeme $ string "-"; e' <- pMultOper'; pAddOper (Oper Minus t e')
             <++ return t

pMultOper' :: ReadP Exp
pMultOper' = do t <- pTerm; pMultOper t;
             <++ pTerm

pMultOper :: Exp -> ReadP Exp
pMultOper t = do lexeme $ string "*"; e' <- pTerm; pMultOper (Oper Times t e')
             <++ do lexeme $ string "//"; e' <- pTerm; pMultOper (Oper Div t e')
             <++ do lexeme $ string "%"; e' <- pTerm; pMultOper (Oper Mod t e')
             <++ return t

pTerm :: ReadP Exp
pTerm = do lexeme $ string"["; ez <- pExprz; lexeme $ string"]"; return (BoaAST.List ez)
        <++ do id <- lexeme pIdent; lexeme $ string "("; ez <- pExprz; lexeme $ string ")"; return (BoaAST.Call id ez);
        <++ do lexeme $ string"["; e <- pExpr; f <- pForClause; c <- pClasuez; lexeme $ string"]"; return (BoaAST.Compr e (f:c))
        <++ do lexeme $ string "("; e <- pExpr; lexeme $ string ")"; return e;
        <++ do pNumConst;
        <++ do pStringConst;
        <++ do lexeme $ string "True"; return $ BoaAST.Const TrueVal
        <++ do lexeme $ string "False"; return $ BoaAST.Const FalseVal
        <++ do lexeme $ string "None"; return $ BoaAST.Const NoneVal
        <++ do i <- lexeme pIdent; return $ BoaAST.Var i

pExprz :: ReadP [Exp]
pExprz = do pExprs;
         <++ return []

pExprs :: ReadP [Exp]
pExprs = do exp1 <- pExpr; lexeme $ char ','; exp2 <- pExprs; return (exp1:exp2)
         <++ do exp1 <- pExpr; return [exp1];

pIf :: ReadP CClause
pIf = do lexeme $ string "if "; exp <- pExpr; return (CCIf exp)

pForClause :: ReadP CClause
pForClause = do lexeme $ string "for "; id <- lexeme pIdent; lexeme $ string "in "; exp <- pExpr; return $ CCFor id exp

pClasuez :: ReadP [CClause]
pClasuez = do f <- lexeme pForClause; c <- pClasuez; return (f:c)
          <++ do i <- lexeme pIf; c <- pClasuez; return (i:c)
          <++ do return []

checkStringConst :: ReadP Char
checkStringConst = do satisfy (\x -> isPrint x && x /= '\'' && x /= '\\')
                   <|> do string "\\'"; return '\''
                   <|> do string "\\\\"; return '\\'
                   <|> do string "\\n"; return '\n'
                   <|> do string "\\\n"; return '\0'

pStringConst :: ReadP Exp
pStringConst = do
    char '\''
    str <- RP.many checkStringConst
    char '\''
    let c = [x | x <- str, x `notElem` "\NUL"]
    return (BoaAST.Const (StringVal c))

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (do skipSpaces; a <- pProgram; eof; return a) s of
  [] -> Left $ ParseError "cannot parse"
  x -> case last x of
    (exp,_) -> Right exp
    -- _ -> Left $ ParseError "error !!!!"