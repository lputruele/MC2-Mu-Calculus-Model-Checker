module Model where

import Types
import ParseLib

data Decl = DVar AP
          | DSeq Decl Decl
          deriving Show

data Comm = Rule (Env,Env)
          | Seq Comm Comm
          deriving Show




--Parser

-- Command parsers

pComm :: [String] -> Parser Comm
pComm xs = (pRule xs) `chainl1` pSemi

pRule :: [String] -> Parser Comm
pRule xs = do {e0 <- pEnv xs; symbol "->"; e1 <- pEnv xs; return (Rule (e0,e1))}

pSemi = symbol ";" >> return Seq

-- Declaration parsers

pDecl :: Parser Decl
pDecl = pVarDecl `chainl1` pDSemi

pVarDecl :: Parser Decl
pVarDecl = do {n <- myident; return (DVar n)}

pDSemi = symbol ";" >> return DSeq

-- Environment parsers

pEnv :: [String] -> Parser Env
pEnv xs = (pFactor xs) `chainl1` pAndOperator

pAndOperator = symbol "," >> return (++)

pFactor :: [String] -> Parser Env
pFactor xs = do {v <- knownVar xs; return [(v,True)]} +++
           do {symbol "!"; v <- knownVar xs; return [(v,False)]} +++ 
           do {return []}

reserved_words :: [String]
reserved_words = ["true", "false", "vars", "rules", "init", "check"]

myident :: Parser String
myident = identifier reserved_words
