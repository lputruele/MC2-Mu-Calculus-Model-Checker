module Mu where

import Data.List
import ParseLib
import Types

--syntax
data Form = Var VName
         | Prop AP
         | And Form Form
         | Or Form Form
         | Not Form
         | Box Form 
         | Diamond Form 
         | Gfp VName Form --Greatest fix-point operator
         | Lfp VName Form --Lowest fix-point operator
         deriving Eq

--printer
instance Show Form where
  show (Var n) = n
  show (Prop p) = p
  show (And f0 f1) = "(" ++ show f0 ++ " & " ++ show f1 ++ ")"
  show (Or f0 f1) = "(" ++ show f0 ++ " | " ++ show f1 ++ ")"
  show (Not f) = "!" ++ show f
  show (Box f) = "[]" ++ show f
  show (Diamond f) = "<>" ++ show f
  show (Gfp n f) = "$" ++ n ++ "." ++ show f
  show (Lfp n f) = "%" ++ n ++ "." ++ show f

sshowList :: [Form] -> String
sshowList [] = ""
sshowList [x] = show x
sshowList (x:xs) = show x ++ "," ++ sshowList xs

--parser

pForm :: [String] -> Parser Form
pForm xs = pVar xs +++ pProp xs +++ pNot xs +++ pBox xs +++ pDiamond xs +++ pGfp xs +++ pLfp xs +++ bracket (symbol "(") (pAnd xs +++ pOr xs) (symbol ")")

pVar :: [String] -> Parser Form
pVar xs = do {symbol ":"; n <- identifier []; return (Var n)}

pProp :: [String] -> Parser Form
pProp xs = do {p <- knownVar xs; return (Prop p)}

pNot :: [String] -> Parser Form
pNot xs = do {symbol "!"; f <- pForm xs; return (Not f)}

pAnd :: [String] -> Parser Form
pAnd xs = do {f0 <- pForm xs; symbol "&"; f1 <- pForm xs; return (And f0 f1)}

pOr :: [String] -> Parser Form
pOr xs = do {f0 <- pForm xs; symbol "|"; f1 <- pForm xs; return (Or f0 f1)}

pBox :: [String] -> Parser Form
pBox xs = do {symbol "[]"; f <- pForm xs; return (Box f)}

pDiamond :: [String] -> Parser Form
pDiamond xs = do {symbol "<>"; f <- pForm xs; return (Diamond f)}

pGfp :: [String] -> Parser Form
pGfp xs = do {symbol "$"; n <- identifier[]; symbol "."; f <- pForm xs; return (Gfp n f)}

pLfp :: [String] -> Parser Form
pLfp xs = do {symbol "%"; n <- identifier[]; symbol "."; f <- pForm xs; return (Lfp n f)}

pForms :: [String] -> Parser [Form]
pForms xs = (pForm xs) `sepby1` symbol ","


