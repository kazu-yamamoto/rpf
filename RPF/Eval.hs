module RPF.Eval (evalRPF) where

import Control.Monad
import Data.List (foldl')
import Data.Maybe
import MailSpec
import RPF.Domain
import RPF.IP
import RPF.Types

----------------------------------------------------------------

evalRPF :: MailSpec -> Policy -> BlockName -> (Pline, Action)
evalRPF ms (Policy bs is ds) bn = evalBlock ms is ds (bs !! fromEnum bn)

----------------------------------------------------------------

evalBlock :: MailSpec -> [IPTable] -> [DomainTable] -> Block -> (Pline, Action)
evalBlock ms is ds (Block _ as) =
    fromJust $ foldl' mplus mzero (map (evalAction ms is ds) as)

----------------------------------------------------------------

evalAction :: MailSpec -> [IPTable] -> [DomainTable] -> ActionCond -> Maybe (Pline, Action)
evalAction _ _ _ (ActionCond l Nothing act) = Just (l, act)
evalAction ms is ds (ActionCond l (Just cnd) act) =
    if evalCnd ms is ds cnd
    then Just (l, act)
    else Nothing

----------------------------------------------------------------

evalCnd :: MailSpec -> [IPTable] -> [DomainTable] -> Cond -> Bool
evalCnd ms is ds (v :== d) = include ms is ds v d
evalCnd ms is ds (v :!= d) = exclude ms is ds v d
evalCnd ms is ds (c1 :&& c2) = evalCnd ms is ds c1 && evalCnd ms is ds c2

----------------------------------------------------------------

include :: MailSpec -> [IPTable] -> [DomainTable] -> Variable -> Constant -> Bool
include ms is _ (DT_IP,  _) (_, CV_Index n) = ipMatch (msPeerIP ms) (is!!n)
include ms _ ds (DT_Dom, vid) (_, CV_Index n) =
    case getDom vid of
      Nothing -> False
      Just dom -> domainMatch dom (ds!!n)
  where
    getDom V_PRA      = msPRA ms
    getDom V_MAILFROM = msMailFrom ms
    getDom V_FROM     = msFrom ms
    getDom V_DKIMFROM = msDKIMFrom ms
    getDom V_DKFROM   = msDKFrom ms
    getDom _          = error "getDom"
include ms _ _ (DT_Res, vid) (_, CV_Result rs) = getRes vid `elem` rs
  where
    getRes V_SPF  = msSPFResult ms
    getRes V_SID  = msSenderIDResult ms
    getRes V_ADSP = msADSPResult ms
    getRes V_DKIM = msDKIMResult ms
    getRes V_DK   = msDKResult ms
    getRes _      = error "getRes"
include ms _ _ (DT_Sig, vid) (_, CV_Sig b) = getSig vid == b
  where
    getSig V_SIGDKIM = msSigDKIM ms
    getSig V_SIGDK   = msSigDK ms
    getSig _         = error "getSig"
include _ _ _ _ _ = error "include"

----------------------------------------------------------------

exclude :: MailSpec -> [IPTable] -> [DomainTable] -> Variable -> Constant -> Bool
exclude a b c d e = not (include a b c d e)
