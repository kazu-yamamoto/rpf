{-# LANGUAGE BangPatterns#-}

module Main where

import Config
import Control.Monad
import Data.IORef
import LogMsg
import Milter
import Network.C10kServer
import Network.DNS
import Network.DomainAuth
import Network.TCPInfo
import RPF
import System.Environment
import System.Exit
import System.IO
import System.Posix hiding (version, Limit)

version :: String
version = "0.2.4"

progName :: String
progName = "rpf " ++ version

----------------------------------------------------------------

main :: IO ()
main = do
    resSeed <- makeResolvSeed defaultResolvConf
    conf <- readFile =<< fileName 0
    polf <- readFile =<< fileName 1
    let !opt        = getOption conf
        !plcy       = parsePolicy polf
        !c10kConfig = toC10kConfig opt
        !lim        = toLimit opt
        !env        = toEnv opt plcy
        !prog       = milterWapper resSeed lim env
    if opt_debug_mode opt
       then runC10kServerH prog c10kConfig
       else daemonize $ runC10kServerH prog c10kConfig
  where
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: rpf config_file policy_file"
            exitFailure
        return $ args !! n

milterWapper :: ResolvSeed -> Limit -> Env -> Handle -> TCPInfo -> IO ()
milterWapper rs lim env hdl _ = do
    hSetBuffering hdl NoBuffering
    st <- newIORef initialState
    withResolver rs $ \resolver -> do
        let env' = env {
                spf  = runSPF lim resolver
              , dk   = runDK' resolver
              , dkim = runDKIM' resolver
              }
        milter env' hdl st

----------------------------------------------------------------

toC10kConfig :: Option -> C10kConfig
toC10kConfig opt = C10kConfig {
    initHook = makeInitHook opt
  , exitHook = makeExitHook
  , parentStartedHook = makeParentHook
  , startedHook = makeStartedHook opt
  , sleepTimer = opt_sleep_timer opt
  , preforkProcessNumber = opt_prefork_process_number opt
  , threadNumberPerProcess = opt_thread_number_per_process opt
  , portName = show $ opt_port opt
  , ipAddr = Just $ opt_addr opt
  , pidFile = opt_pid_file opt
  , user = opt_user opt
  , group = opt_group opt
}

makeInitHook :: Option -> IO ()
makeInitHook opt =
  if opt_debug_mode opt
  then initLog progName "" (opt_log_level opt) StdErr
  else initLog progName (opt_syslog_facility opt) (opt_log_level opt) SysLog

makeExitHook :: String -> IO ()
makeExitHook = errorMsg

makeParentHook :: IO ()
makeParentHook = infoMsg $ progName ++ " started"

makeStartedHook :: Option -> IO ()
makeStartedHook opt =
  if opt_debug_mode opt
  then initLog progName "" (opt_log_level opt) StdErr
  else initLog progName (opt_syslog_facility opt) (opt_log_level opt) SysLog

----------------------------------------------------------------

toLimit :: Option -> Limit
toLimit opt = Limit {
    limit = opt_include_redirect_limit opt
  , ipv4_masklen = opt_minimum_ipv4_mask_length opt
  , ipv6_masklen = opt_minimum_ipv6_mask_length opt
  , reject_plus_all = opt_reject_plus_all opt
  }

toEnv :: Option -> Policy -> Env
toEnv opt plcy = defaultEnv {
    policy  = plcy
  , logonly = opt_logonly opt
  , debug   = opt_debug_mode opt
  , errorHook   = warnMsg
  , resultHook  = noticeMsg
  , monitorHook = infoMsg
  , debugHook   = debugMsg
  }

----------------------------------------------------------------

daemonize :: IO () -> IO ()
daemonize program = ensureDetachTerminalCanWork $ do
    detachTerminal
    ensureNeverAttachTerminal $ do
        changeWorkingDirectory "/"
        setFileCreationMask 0
        mapM_ closeFd [stdInput, stdOutput, stdError]
        program
  where
    ensureDetachTerminalCanWork p = do
        forkProcess p
        exitImmediately ExitSuccess
    ensureNeverAttachTerminal p = do
        forkProcess p
        exitImmediately ExitSuccess
    detachTerminal = createSession
