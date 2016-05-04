#!/usr/bin/runghc
module Main where

import Data.List
import System.IO
import System.Environment
import OBDD
import OBDD.Data
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import ParseLib
import Mu
import MuEval
import Types
import Model
import ModelEval


pProg :: Parser (Env,[Form],OBDD AP)
pProg = do {symbol "vars"; d <- pDecl; symbol "rules"; c <- pComm; symbol "init"; e <- pEnv; 
            symbol "check"; f <- pForms; return (deval d e,f,(ceval c (deval d [])))} 

inst :: Env -> OBDD AP -> OBDD AP
inst [] obdd = obdd
inst (x:xs) obdd = inst xs (OBDD.instantiate (fst x)(snd x) obdd)

-- exec
exec :: (Env,[Form], OBDD AP) -> [Form]
exec (v,[],obdd) = []
exec (v,(f:fs), obdd) = if OBDD.null  (OBDD.not(inst v (check f v obdd assoc0 False))) then f : exec (v,fs,obdd) 
                        else exec (v,fs,obdd)


showResult :: [Form] -> IO ()
showResult [] = putStr ("\n")
showResult (f:fs)  = do 
                        putStr ("Model satisfies property: (" ++ show f ++ ") \n")
                        showResult fs

-- Parse and exec from file

nullCheck n l = if Data.List.null l then return () else putStr n
notnullCheck n l = if Data.List.null l then putStr n else return ()

ioCheck :: String -> IO (Env,[Form], OBDD AP)
ioCheck filename = do { src <- readFile filename;
                        ps <- return (papply (parse pProg) src);
                        notnullCheck "Parse error\n" ps;
                        (e,rest) <- return (head ps);
                        nullCheck ("Parse error before :"++rest++"\n") rest;
                        return e
                      }

exec_from_file :: String -> IO () 
exec_from_file filename = do
                             e <- ioCheck ("../test/"++filename)
                             showResult (exec e)

-- execute from command line
main :: IO ()
main = do
          a <- getArgs
          if (a == []) then putStr "Usage: ./Main.hs model.\n"
                       else exec_from_file (head a)

