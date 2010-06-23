module Milter.Types where

import MailSpec
import Network.DomainAuth

----------------------------------------------------------------

data State = State {
    xmail    :: XMail
  , mailspec :: MailSpec
  , prdspec  :: PRD
  , parsedv  :: ParsedValue
  } deriving Show

initialState :: State
initialState = State {
    xmail    = initialMail
  , mailspec = initialMailSpec
  , prdspec  = initialPRD
  , parsedv  = initialParsedValue
  }

----------------------------------------------------------------

data ParsedValue = ParsedValue {
    mpdk   :: Maybe DK
--  , mpdkim :: Maybe DKIM
  } deriving Show

initialParsedValue :: ParsedValue
initialParsedValue = ParsedValue {
    mpdk = Nothing
  }
