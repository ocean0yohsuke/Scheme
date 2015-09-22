module Scheme.Parser (
    Filename, ParseError,
    readSchemeExpr, 
    readSchemeCode,
    readSchemeFile,
    ) where

import Scheme.DataType
import MonadX.Applicative hiding ((<|>), many)
import MonadX.Monad

import Data.Char
import Text.Parsec hiding (spaces)
import Text.Parsec.String

type Filename = String

----------------------------------------------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------------------------------------------

hookParserState :: Parser a -> Parser (a, String)
hookParserState p = do
    expr <- p 
    rest <- stateInput |$> getParserState
    (*:) (expr, rest)

readSchemeExpr :: String -> Either ParseError (Expr, String)
readSchemeExpr = parse (hookParserState pExpr) "scheme expression" 

readSchemeCode :: String -> Either ParseError (ScmCode, String)
readSchemeCode = parse (hookParserState pCode) "scheme code" 

readSchemeFile :: Filename -> String -> Either ParseError (ScmFile, String)
readSchemeFile filename = parse (hookParserState pFile) filename

----------------------------------------------------------------------------------------------------------------
-- Main parsers for each reader
----------------------------------------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = pAtom <|> pQuotekind <|> pCELL 

pCode :: Parser ScmCode
pCode = spaces *> (commentb <|> linebreak <|> expr) <* spaces
  where
    expr = do
        expr <- pExpr 
        (*:) $ EXPR expr
    commentb = do
        cb <- commentblock
        (*:) $ COMMENT cb
    linebreak = do
        newline
        (*:) LINEBREAK

pFile :: Parser ScmFile
pFile = do
    c <- pCode
    cs <- many pCode
    eof
    (*:) $ (c:cs)

----------------------------------------------------------------------------------------------------------------
-- Preliminary 
----------------------------------------------------------------------------------------------------------------

getSourcePos :: Parser SourcePos
getSourcePos = statePos |$> getParserState

symbol :: Parser Char
symbol = oneOf "!#$&|:<=>?@_~%*+-/^"

delimiter :: String
delimiter = " \t\n"

spaces :: Parser ()
spaces = skipMany $ oneOf " \t"

newlines :: Parser ()
newlines = skipMany newline

spacelines :: Parser ()
spacelines = skipMany $ oneOf delimiter

commentline :: Parser String
commentline = do
    char ';' 
    manyTill anyChar (try newline)

commentblock :: Parser String
commentblock = do
    c <- commentline <* spaces
    cs <- many $ commentline <* spaces
    (*:) $ (c ++ concat cs)

comspacelines :: Parser ()
comspacelines = spacelines >> (skipMany $ spacelines *> commentblock <* spacelines)

parens :: Parser a -> Parser a
parens p = betweenparens ('(',')') p 
       <|> betweenparens ('[',']') p
       <|> betweenparens ('{','}') p
  where
    betweenparens :: (Char, Char) -> Parser a -> Parser a 
    betweenparens (l,r) p = between (char l <* comspacelines) 
                                    (comspacelines *> char r) 
                                    p

tryparens :: Parser Expr -> Parser Expr
tryparens p = try $ parens p

------------------------------------
-- pAtomPI
------------------------------------

pNIL :: Parser Expr
pNIL = try nil <?> "nil"
  where
    nil = do
        string "()"        
        (*:) NIL
      
pINT :: Parser Expr
pINT = try (pPlus <|> pMinus) <?> "integer"
  where
    pPlus = do
        i <- many1 digit
        (*:) $ INT (read i)
    pMinus = do
        char '-'
        INT n <- pPlus
        (*:) $ INT (n*(-1))
pREAL :: Parser Expr
pREAL = try (pPlus <|> pMinus) <?> "real number"
  where
    pPlus = do
        l <- many1 digit 
        char '.'
        r <- many1 digit 
        (*:) $ REAL (read $ l ++ "." ++ r)
    pMinus = do
        char '-'
        REAL n <- pPlus
        (*:) $ REAL (n*(-1))
pNUM :: Parser Expr
pNUM = try (pINT <* notFollowedBy (char '.')) <|> pREAL 

pSYM :: Parser Expr
pSYM = try (name <|> try reference <|> operator) <?> "symbol" 
  where
    name = do
        sp <- getSourcePos
        c <- letter
        cs <- many $ letter <|> symbol <|> digit <|> oneOf "'"
        (*:) $ SYM (c:cs) (Just sp)
    reference = do
        sp <- getSourcePos
        c <- symbol
        ls <- many $ symbol <|> digit <|> oneOf "'"
        cs <- many1 $ letter
        rs <- many $ letter <|> symbol <|> digit <|> oneOf "'"
        (*:) $ SYM (c:(ls++cs++rs)) (Just sp)
    operator = do
        sp <- getSourcePos
        notFollowedBy $ string "->" 
        cs <- many1 $ symbol <|> oneOf ""
        (*:) $ SYM cs (Just sp)

pSTR :: Parser Expr
pSTR = try str <?> "string" 
  where
    str = do 
        char '"'
        x <- many (noneOf "\"")
        char '"'
        (*:) $ STR x

pAtom :: Parser Expr
pAtom = pNIL <|> pNUM <|> pSYM <|> pSTR

------------------------------------
-- pQuotekind
------------------------------------

pQuote :: Parser Expr
pQuote = try quote <?> "quote"
  where
    quote = do
        sp <- getSourcePos
        char '\''
        sp' <- getSourcePos
        expr <- pExpr
        (*:) $ CELL (SYM "quote" (Just sp)) (CELL expr NIL (Just sp')) (Just sp)

pQuasiquote :: Parser Expr
pQuasiquote = try quasiquote <?> "quasiquote"
  where
    quasiquote = do
        sp <- getSourcePos
        char '`'
        sp' <- getSourcePos
        expr <- pExpr
        (*:) $ CELL (SYM "quasiquote" (Just sp)) (CELL expr NIL (Just sp')) (Just sp)
       
pUnquote :: Parser Expr
pUnquote = try unquote <?> "unquote"
  where
    unquote = do
        sp <- getSourcePos
        char ','
        sp' <- getSourcePos
        expr <- pExpr
        (*:) $ CELL (SYM "unquote" (Just sp)) (CELL expr NIL (Just sp')) (Just sp)

pUnquoteSplicing :: Parser Expr
pUnquoteSplicing = try unquote_splicing <?> "unquote-splicing"
  where
    unquote_splicing = do
        sp <- getSourcePos
        string ",@"
        sp' <- getSourcePos
        expr <- pExpr
        (*:) $ CELL (SYM "unquote-splicing" (Just sp)) (CELL expr NIL (Just sp')) (Just sp)

pQuotekind :: Parser Expr
pQuotekind = pQuote <|> pQuasiquote <|> pUnquoteSplicing <|> pUnquote 


------------------------------------
-- pCell
------------------------------------

pCELL :: Parser Expr
pCELL = do
    sp <- getSourcePos
    tryparens (pList (Just sp)) <|> tryparens (pDottedList (Just sp)) <?> "cell"
  where
    pList msp = 
        CELL |$> pExpr
             |*> (try (oneOf delimiter *> comspacelines *> pList msp) <|> (*:) NIL)
             |*  msp
    pDottedList msp = 
        CELL |$> pExpr
             |*> (try (oneOf delimiter *> comspacelines *> pDottedList msp) <|> pDotExpr)
             |*  msp
      where
        pDotExpr = do
            newlines *> oneOf delimiter <* spacelines
            char '.' 
            newlines *> oneOf delimiter <* spacelines
            pExpr

------------------------------------
-- pCLOS
------------------------------------

pCLOS :: Parser Expr
pCLOS = do
    sp <- getSourcePos
    tryparens (pClos (Just sp)) <?> "clos"
  where
    pClos msp = do
        char '\\'
        parms <- try pList <|> try pDottedList <|> (*:) NIL
        string " -> "
        seq <- pSeq 
        --(*:) $ CELL (SYM "lambda") (CELL parms NIL) seq 
        (*:) $ CLOS (CELL parms seq msp) Nothing 
      where
        pList = 
            CELL |$> pSYM
                 |*> (try (char ' ' *> pList) <|> (*:) NIL)
                 |*  msp
        pDottedList = 
            CELL |$> pSYM
                 |*> (try (char ' ' *> pDottedList) <|> pDotExpr)
                 |*  msp
          where
            pDotExpr = string " . "  *> pExpr
        pSeq = CELL |$> pExpr
                    |*> (try (string " >> " *> pSeq) <|> (*:) NIL)
                    |*  msp


