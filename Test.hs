{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Test where

import Control.Applicative
import Data.IP
import Data.IP.RouteTable as T
import Data.Map as M
import Network.DomainAuth
import RPF.IP
import RPF.Parser
import RPF.Types
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime
import Test.HUnit

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_policy1 :: Assertion
case_policy1 = do
    plcy <- parsePolicy <$> readFile "config/rpf.policy"
    plcy @?= res
  where
    res = Policy [
        Block B_Connect [ActionCond 3 (Just ((DT_IP,V_IP) :== (DT_IP,CV_Index 0))) A_Accept,ActionCond 4 Nothing A_Continue]
      , Block B_MailFrom [ActionCond 9 (Just ((DT_Res,V_SPF) :== (DT_Res,CV_Result [DAPass]))) A_Accept,ActionCond 10 Nothing A_Continue]
      , Block B_Header [ActionCond 15 (Just ((DT_Res,V_SID) :== (DT_Res,CV_Result [DAPass]))) A_Accept,ActionCond 16 (Just (((DT_Dom,V_MAILFROM) :== (DT_Dom,CV_Index 0)) :&& ((DT_Sig,V_SIGDK) :== (DT_Sig,CV_Sig False)))) A_Reject,ActionCond 17 Nothing A_Continue]
      , Block B_Body [ActionCond 22 (Just ((DT_Res,V_DKIM) :== (DT_Res,CV_Result [DAPass]))) A_Accept,ActionCond 23 (Just ((DT_Res,V_DK) :== (DT_Res,CV_Result [DAPass]))) A_Accept,ActionCond 24 Nothing A_Continue]
      ]
          [IPTable (T.fromList [(makeAddrRange (toIPv4 [127,0,0,1]) 32,True)]) T.empty]
          [M.fromList [("yahoo.com",True)]]

