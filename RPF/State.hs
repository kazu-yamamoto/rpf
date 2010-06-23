module RPF.State where

import Data.IP
import Network.DNS.Types (Domain)
import Network.DomainAuth
import RPF.Types

type ConstName = String
type ConstTable  = [(ConstName, Constant)]

data ParserState = ParserState {
    consttbl :: ConstTable
  , used     :: [ConstName]
  , unused   :: [ConstName]
  , iplol    :: [[IPRange]]
  , ipcnt    :: Int
  , domlol   :: [[Domain]]
  , domcnt   :: Int
  , reslol   :: [[DAResult]]
  , rescnt   :: Int
  , blocks   :: [BlockName]
  } deriving Show

initialState :: ParserState
initialState = ParserState {
    consttbl = []
  , used     = []
  , unused   = []
  , iplol    = []
  , ipcnt    = 0
  , domlol   = []
  , domcnt   = 0
  , reslol   = []
  , rescnt   = 0
  , blocks   = [minBound..maxBound]
  }
