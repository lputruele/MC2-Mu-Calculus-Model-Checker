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

pForm :: Parser Form
pForm = pVar +++ pProp +++ pNot +++ pBox +++ pDiamond +++ pGfp +++ pLfp +++ bracket (symbol "(") (pAnd +++ pOr) (symbol ")")

pVar :: Parser Form
pVar = do {symbol ":"; n <- identifier []; return (Var n)}

pProp :: Parser Form
pProp = do {p <- identifier []; return (Prop p)}

pNot :: Parser Form
pNot = do {symbol "!"; f <- pForm; return (Not f)}

pAnd :: Parser Form
pAnd = do {f0 <- pForm; symbol "&"; f1 <- pForm; return (And f0 f1)}

pOr :: Parser Form
pOr = do {f0 <- pForm; symbol "|"; f1 <- pForm; return (Or f0 f1)}

pBox :: Parser Form
pBox = do {symbol "[]"; f <- pForm; return (Box f)}

pDiamond :: Parser Form
pDiamond = do {symbol "<>"; f <- pForm; return (Diamond f)}

pGfp :: Parser Form
pGfp = do {symbol "$"; n <- identifier[]; symbol "."; f <- pForm; return (Gfp n f)}

pLfp :: Parser Form
pLfp = do {symbol "%"; n <- identifier[]; symbol "."; f <- pForm; return (Lfp n f)}

pForms :: Parser [Form]
pForms = pForm `sepby1` symbol ","
