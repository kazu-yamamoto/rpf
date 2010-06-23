module RPF.Domain where

import Data.Map (Map)
import qualified Data.Map as M hiding (Map)
import Data.Maybe
import Network.DNS.Types (Domain)

type DomainTable = Map Domain Bool

domainMatch :: Domain -> DomainTable -> Bool
domainMatch dom = fromMaybe False . M.lookup dom

makeDomainTable :: [Domain] -> DomainTable
makeDomainTable ds = M.fromList $ zip ds (repeat True)



