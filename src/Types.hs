module Types where

import Data.List
import OBDD

type VName = String
type AP = String
type Env = [(AP,Bool)]
type Assoc = VName -> OBDD AP

assoc0 :: Assoc
assoc0 = \n -> OBDD.constant False

update :: (VName,OBDD AP) -> Assoc -> Assoc
update (n,v) a = \m -> if m == n then v else a m 

