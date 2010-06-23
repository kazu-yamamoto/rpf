module RPF.Types where

import Network.DNS.Types (Domain)
import Network.DomainAuth
import RPF.IP

type Pline = Int

----------------------------------------------------------------

data Action = A_Accept | A_Discard | A_Hold | A_Reject | A_Continue deriving (Eq, Enum, Bounded, Show)

{-
instance Show Action where
    show act = "\"" ++ actionWords !! fromEnum act ++ "\""
-}

actionWords :: [String]
actionWords = ["accept","discard","hold","reject","continue"]

----------------------------------------------------------------

data DataType = DT_Sig | DT_Res | DT_Dom | DT_IP deriving (Eq, Show)

type Variable = (DataType, VariableId)

data VariableId = V_SIGDKIM | V_SIGDK
                | V_SPF | V_SID | V_DKIM | V_DK | V_ADSP
                | V_MAILFROM | V_FROM | V_PRA | V_DKIMFROM | V_DKFROM
                | V_IP
                deriving (Eq, Enum, Bounded, Show)

{-
instance Show VariableId where
    show vid = "\"" ++ variableNames !! fromEnum vid ++ "\""
-}

variableNames :: [String]
variableNames = [ "sig_dkim", "sig_domainkeys"
                , "spf", "sender_id", "dkim", "domainkeys", "adsp"
                , "mail_from", "from", "pra", "dkim_from", "domainkeys_from"
                , "ip"
                ]

variableTypes :: [DataType]
variableTypes = [ DT_Sig, DT_Sig
                , DT_Res, DT_Res, DT_Res, DT_Res, DT_Res
                , DT_Dom, DT_Dom, DT_Dom, DT_Dom, DT_Dom
                , DT_IP
                ]

varSanity :: [(BlockName, [VariableId])]
varSanity = [(B_Connect,  [V_IP]),
             (B_MailFrom, [V_IP, V_MAILFROM, V_SPF]),
             (B_Header,   [V_IP, V_MAILFROM, V_SPF, V_FROM, V_PRA, V_SID, V_DKIMFROM, V_DKFROM, V_SIGDKIM, V_SIGDK]),
             (B_Body,     [V_IP, V_MAILFROM, V_SPF, V_FROM, V_PRA, V_SID, V_DKIMFROM, V_DKFROM, V_SIGDKIM, V_SIGDK, V_DKIM, V_DK, V_ADSP])]

resultSanity :: [(VariableId, [DAResult])]
resultSanity =  [(V_SPF,  [DANone, DANeutral, DAPass, DAPolicy, DAHardFail, DASoftFail, DATempError, DAPermError]),
                 (V_SID,  [DANone, DANeutral, DAPass, DAPolicy, DAHardFail, DASoftFail, DATempError, DAPermError]),
                 (V_DKIM, [DANone, DAPass, DAFail, DAPolicy, DANeutral, DATempError, DAPermError]),
                 (V_DK,   [DANone, DAPass, DAFail, DAPolicy, DANeutral, DATempError, DAPermError]),
                 (V_ADSP, [DANone, DAPass, DAUnknown, DAFail, DADiscard, DANxDomain, DATempError, DAPermError])]


----------------------------------------------------------------

resultWords :: [String]
resultWords = [
    "pass", "hardfail", "softfail", "neutral"
  , "fail", "temperror", "permerror", "none"
  , "policy", "nxdomain", "discard", "unknown"
  ]

----------------------------------------------------------------

data BlockName = B_Connect | B_MailFrom | B_Header | B_Body deriving (Eq, Enum, Bounded, Show)

blockNames :: [String]
blockNames = ["connect", "mail_from", "header", "body"]

{-
instance Show BlockName where
    show blknm = "\"" ++ blockNames !! fromEnum blknm ++ "\""
-}

----------------------------------------------------------------

noyes :: [String]
noyes = ["No", "Yes"]

----------------------------------------------------------------

type Constant = (DataType, ConstantValue)
data ConstantValue = CV_Index Int | CV_Result [DAResult] | CV_Sig Bool
                     deriving (Eq, Show)

----------------------------------------------------------------

data Policy = Policy [Block] [IPTable] [[Domain]] deriving (Eq, Show)
data Block = Block BlockName [ActionCond] deriving (Eq, Show)
data ActionCond = ActionCond Pline (Maybe Cond) Action deriving (Eq, Show)
data Cond = Variable :== Constant | Variable :!= Constant | Cond :&& Cond deriving (Eq, Show)
