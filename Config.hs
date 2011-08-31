{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Config (Option(..), getOption, defaultOption) where

import Control.Applicative hiding (many,optional,(<|>))
import Data.List (isPrefixOf)
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------

defaultOption :: Option
defaultOption = Option {
    opt_addr = "127.0.0.1"
  , opt_port = 10025
  , opt_reject_plus_all = True
  , opt_minimum_ipv4_mask_length = 16
  , opt_minimum_ipv6_mask_length = 48
  , opt_include_redirect_limit = 10
  , opt_syslog_facility = "local4"
  , opt_log_level = "notice"
  , opt_logonly = False
  , opt_debug_mode = False
  , opt_prefork_process_number = 4
  , opt_thread_number_per_process = 500
  , opt_sleep_timer = 2
  , opt_user = "nobody"
  , opt_group = "nobody"
  , opt_pid_file = "/var/run/rpf.pid"
  }

data Option = Option {
    opt_addr :: String
  , opt_port :: Int
  , opt_reject_plus_all :: Bool
  , opt_minimum_ipv4_mask_length :: Int
  , opt_minimum_ipv6_mask_length :: Int
  , opt_include_redirect_limit :: Int
  , opt_syslog_facility :: String
  , opt_log_level :: String
  , opt_logonly :: Bool
  , opt_debug_mode :: Bool
  , opt_prefork_process_number :: Int
  , opt_thread_number_per_process :: Int
  , opt_sleep_timer :: Int
  , opt_user :: String
  , opt_group :: String
  , opt_pid_file :: String
  } deriving Show

----------------------------------------------------------------

getOption :: String -> Option
getOption = makeOpt defaultOption . parseConfig

----------------------------------------------------------------

makeOpt :: Option -> [Conf] -> Option
makeOpt def conf = Option {
    opt_addr = get "Address" opt_addr
  , opt_port = get "Port" opt_port
  , opt_reject_plus_all = get "Reject_Plus_All" opt_reject_plus_all
  , opt_minimum_ipv4_mask_length = get "Minimum_IPv4_Mask_Length" opt_minimum_ipv4_mask_length
  , opt_minimum_ipv6_mask_length = get "Minimum_IPv6_Mask_Length" opt_minimum_ipv6_mask_length
  , opt_include_redirect_limit = get "Include_Redirect_Limit" opt_include_redirect_limit
  , opt_syslog_facility = get "Syslog_Facility" opt_syslog_facility
  , opt_log_level = get "Log_Level" opt_log_level
  , opt_logonly = get "Logonly" opt_logonly
  , opt_debug_mode = get "Debug" opt_debug_mode
  , opt_prefork_process_number = get "Prefork_Process_Number" opt_prefork_process_number
  , opt_thread_number_per_process = get "Thread_Number_Per_Process" opt_thread_number_per_process
  , opt_sleep_timer      = get "Sleep_Timer" opt_sleep_timer
  , opt_user             = get "User" opt_user
  , opt_group            = get "Group" opt_group
  , opt_pid_file         = get "Pid_File" opt_pid_file
  }
    where
      get key func = maybe (func def) fromConf $ lookup key conf

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Int Int | CV_Bool Bool | CV_String String deriving Show

class FromConf a where
    fromConf :: ConfValue -> a

instance FromConf Int where
    fromConf (CV_Int n) = n
    fromConf _ = error "fromConf int"

instance FromConf Bool where
    fromConf (CV_Bool b) = b
    fromConf _ = error "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = s
    fromConf _ = error "fromConf string"

----------------------------------------------------------------

parseConfig :: String -> [Conf]
parseConfig cs = map parseConf css
    where
      css = filter (not.isPrefixOf "#") . lines $ cs
      parseConf xs = case parse config "config" xs of
                       Right cnf -> cnf
                       Left  err -> error $ show err

----------------------------------------------------------------

config :: Parser Conf
config = (,) <$> name <*> (spaces >> char ':' >> spaces *> value)

name :: Parser String
name = many1.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

value :: Parser ConfValue
value = choice [try cv_int, try cv_bool, cv_string]

cv_int :: Parser ConfValue
cv_int = CV_Int . read <$> (many1 digit <* (spaces >> eof))

cv_bool :: Parser ConfValue
cv_bool = CV_Bool True  <$ (string "Yes" >> spaces >> eof) <|>
          CV_Bool False <$ (string "No"  >> spaces >> eof)

cv_string :: Parser ConfValue
cv_string = CV_String <$> many1 (noneOf " \t\n")
