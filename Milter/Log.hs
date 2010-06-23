module Milter.Log where

import Control.Applicative
import Data.IORef
import MailSpec
import Milter.Env
import Milter.Types

----------------------------------------------------------------

logError :: Env -> IORef State -> String -> IO ()
logError env st msg = do
    msg' <- addIP st msg
    errorHook env msg'

logResult :: Env -> IORef State -> String -> IO ()
logResult env st msg = do
    msg' <- addIP st msg
    let msg'' = msg' ++ if logonly env then " (log only)" else ""
    resultHook env msg''

logMonitor :: Env -> IORef State -> String -> IO ()
logMonitor env st msg = do
    msg' <- addIP st msg
    monitorHook env msg'

logDebug :: Env -> IORef State -> String -> IO ()
logDebug env st msg = do
    msg' <- addIP st msg
    debugHook env msg'

addIP :: IORef State -> String -> IO String
addIP st msg = do
    ip <- msPeerIP . mailspec <$> readIORef st
    return $ show ip ++ " " ++ msg
