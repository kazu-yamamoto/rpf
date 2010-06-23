module MailSpec where

import Data.IP
import Network.DNS.Types (Domain)
import Network.DomainAuth

----------------------------------------------------------------

data MailSpec = MailSpec {
    msPeerIP         :: IP           -- #ip
  , msMailFrom       :: Maybe Domain -- #mail_from
  , msFrom           :: Maybe Domain -- #from
  , msPRA            :: Maybe Domain -- #pra
  , msDKIMFrom       :: Maybe Domain -- #dkim_from
  , msDKFrom         :: Maybe Domain -- #domainkeys_from
  , msSPFResult      :: DAResult     -- #spf
  , msSenderIDResult :: DAResult     -- #sender_id
  , msDKIMResult     :: DAResult     -- #dkim
  , msDKResult       :: DAResult     -- #domainkeys
  , msADSPResult     :: DAResult     -- #adsp
  , msSigDKIM        :: Bool         -- #sig_dkim
  , msSigDK          :: Bool         -- #sig_domainkeys
  } deriving Show

initialMailSpec :: MailSpec
initialMailSpec = MailSpec {
    msPeerIP         = IPv4 $ read "127.0.0.1" -- xxx
  , msMailFrom       = Nothing
  , msFrom           = Nothing
  , msPRA            = Nothing
  , msDKIMFrom       = Nothing
  , msDKFrom         = Nothing
  , msSPFResult      = DANone
  , msSenderIDResult = DANone
  , msDKIMResult     = DANone
  , msDKResult       = DANone
  , msADSPResult     = DANone
  , msSigDKIM        = False
  , msSigDK          = False
  }
