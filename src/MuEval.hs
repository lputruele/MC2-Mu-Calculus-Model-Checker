module MuEval where

import Mu
import OBDD
import Types
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

--Model Checker
--v = variables, m = model, e = RV Environment, x = isNextState?


check :: Form -> Env ->  OBDD AP -> Assoc -> Bool -> OBDD AP

check (Prop p) v m e x = if Prelude.not x then OBDD.unit p True 
						 				  else OBDD.unit (p++"'") True

check (Var n) v m e x = if Prelude.not x then e n 
	                                     else rewriteMany v (e n)

check (Not f) v m e x = OBDD.not (check f v m e x) 

check (And f0 f1) v m e x = OBDD.and[check f0 v m e x,check f1 v m e x]

check (Or f0 f1) v m e x = OBDD.or[check f0 v m e x,check f1 v m e x]

check (Diamond f) v m e x = exists_many (Set.fromList [(fst y++"'") | y <- v]) (OBDD.and[m,check f v m e True])

check (Box f) v m e x = check (Not (Diamond (Not f))) v m e x

check (Lfp n f) v m e x =  let res = (OBDD.constant False) in fix n f v m (update (n,res) e) x (check f v m (update (n,res) e) x) res
						
check (Gfp n f) v m e x =  let res = (OBDD.constant True) in fix n f v m (update (n,res) e) x (check f v m (update (n,res) e) x) res

fix :: VName -> Form -> Env -> OBDD AP -> Assoc -> Bool -> OBDD AP -> OBDD AP -> OBDD AP
fix n f v m e x res old = let tmp = OBDD.and[OBDD.or[OBDD.not old,res],OBDD.or[OBDD.not res,old]] in
                          if OBDD.null(OBDD.not tmp) -- if res and old are equivalent
                          	then res
						    else fix n f v m (update (n,res) e) x (check f v m (update (n,res) e) x) res

rewriteMany :: Env -> OBDD AP -> OBDD AP
rewriteMany [] obdd = obdd
rewriteMany (x:xs) obdd = rewriteMany xs (OBDD.rewrite(fst x)(fst x ++ "'") obdd)           