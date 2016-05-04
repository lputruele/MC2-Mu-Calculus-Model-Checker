module Model where

import Types
import ParseLib

data Decl =  DVar VName
            | DSeq Decl Decl
          deriving Show

data Comm = Rule (Env,Env)
          | Seq Comm Comm
          deriving Show




--Parser

-- Command parsers

pComm :: Parser Comm
pComm = pRule `chainl1` pSemi

pRule :: Parser Comm
pRule = do {e0 <- pEnv; symbol "->"; e1 <- pEnv; return (Rule (e0,e1))}

pSemi = symbol ";" >> return Seq

-- Declaration parsers

pDecl :: Parser Decl
pDecl = pVarDecl `chainl1` pDSemi

pVarDecl :: Parser Decl
pVarDecl = do {n <- myident; return (DVar n)}

pDSemi = symbol ";" >> return DSeq

-- Environment parsers

pEnv :: Parser Env
pEnv = pFactor `chainl1` pAndOperator

pAndOperator = symbol "," >> return (++)

pFactor :: Parser Env
pFactor = do {v <- myident; return [(v,True)]} +++
          do {symbol "!"; v <- myident; return [(v,False)]} +++ 
          do {return []}

reserved_words :: [String]
reserved_words = ["true", "false", "vars", "rules", "init", "check"]

myident :: Parser String
myident = identifier reserved_words
