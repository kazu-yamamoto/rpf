{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Applicative
import Data.Map
import Network.DomainAuth
import RPF.Parser
import RPF.Types
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

tests :: [Test]
tests = [
    testGroup "Policy" [
         testCase "policy1" test_policy1
--       , testCase "policy2" test_policy2
       ]
  ]

----------------------------------------------------------------

test_policy1 :: Assertion
test_policy1 = do
    plcy <- parsePolicy <$> readFile "data/policy1"
    plcy @?= res
  where
    res = Policy [
        Block B_Connect [
             ActionCond 2 Nothing A_Continue
           ]
      , Block B_MailFrom [
             ActionCond 6 (Just ((DT_Res,V_SPF) :== (DT_Res,CV_Result [DASoftFail,DAHardFail]))) A_Reject
           , ActionCond 7 Nothing A_Continue
           ]
      , Block B_Header [
             ActionCond 11 (Just ((DT_Res,V_SID) :== (DT_Res,CV_Result [DAPass]))) A_Accept
           , ActionCond 12 (Just (((DT_Dom,V_MAILFROM) :== (DT_Dom,CV_Index 0)) :&& ((DT_Sig,V_SIGDK) :== (DT_Sig,CV_Sig False)))) A_Reject
           , ActionCond 13 Nothing A_Continue
           ]
      , Block B_Body [
             ActionCond 17 (Just ((DT_Res,V_DKIM) :== (DT_Res,CV_Result [DAPass]))) A_Accept
           , ActionCond 18 (Just ((DT_Res,V_DK) :== (DT_Res,CV_Result [DAPass]))) A_Accept
           , ActionCond 19 Nothing A_Continue]
      ]
          [] [fromList [("yahoo.com",True)]]

----------------------------------------------------------------

main :: IO ()
main = defaultMain tests
