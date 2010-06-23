module Milter.Env where

import Data.IP
import Network.DNS
import Network.DomainAuth
import RPF

data Env = Env {
    policy   :: Policy
  , logonly  :: Bool
  , debug    :: Bool
  , spf      :: Domain -> IP -> IO DAResult
  , dk       :: Mail -> DK -> IO DAResult
  , errorHook   :: String -> IO ()
  , resultHook  :: String -> IO ()
  , monitorHook :: String -> IO ()
  , debugHook   :: String -> IO ()
  }

defaultEnv :: Env
defaultEnv = Env {
    policy  = undefined
  , logonly = undefined
  , debug   = undefined
  , spf     = undefined
  , dk      = undefined
  , errorHook   = undefined
  , resultHook  = undefined
  , monitorHook = undefined
  , debugHook   = undefined
  }
