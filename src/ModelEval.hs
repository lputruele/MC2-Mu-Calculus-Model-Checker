module ModelEval where

import Types
import Model
import OBDD
import Data.List

--Generates OBDD from a model of a system
ceval :: Comm -> Env -> OBDD AP
ceval (Seq c0 c1) env = let (x,y) = (ceval c0 env,ceval c1 env) in OBDD.or[x,y] 
ceval (Rule (e0,e1)) env = let xs = gentrans (filt env e0) [] in 
							  if xs /= [] then 
							    OBDD.or[OBDD.and[eeval x, eeval2 (fill x e1)]| x <- xs]
							  else 
                                OBDD.and[eeval e0, eeval2 (fill e0 e1)]

eeval :: Env -> OBDD AP
eeval [] = OBDD.constant True
eeval (x:xs) = if snd x then OBDD.and[OBDD.unit (fst x) True, eeval xs]
					  else OBDD.and[OBDD.unit (fst x) False, eeval xs]

eeval2 :: Env -> OBDD AP
eeval2 [] = OBDD.constant True
eeval2 (x:xs) = if snd x then OBDD.and[OBDD.unit (fst x++"'") True, eeval2 xs]
					   else OBDD.and[OBDD.unit (fst x++"'") False, eeval2 xs]

fill :: Env -> Env -> Env
fill [] ys = ys
fill (x:xs) ys = if fst x `notElem` map fst ys then fill xs ([x] `union` ys)
											 else fill xs ys

filt :: Env -> Env -> Env
filt [] ys = []
filt (x:xs) ys = if fst x `notElem` map fst ys then [x] `union` filt xs ys
											 else filt xs ys

gentrans :: Env -> Env -> [Env]
gentrans [] ys = [ys]
gentrans (x:xs) ys = gentrans xs ((fst x,False):ys) ++ gentrans xs ((fst x,True):ys)

--Declaration eval
deval :: Decl -> Env -> Env
deval (DVar n) env = if (n,True) `elem` env then [(n,True)] else [(n,False)]
deval (DSeq d0 d1) env = deval d0 env ++ deval d1 env




