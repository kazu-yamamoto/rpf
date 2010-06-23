module RPF.IP where

import Data.IP
import Data.IP.RouteTable (IPRTable)
import qualified Data.IP.RouteTable as T hiding (IPRTable)
import Data.Maybe

----------------------------------------------------------------

data IPTable = IPTable {
    ipv4table :: IPRTable IPv4 Bool -- Bool is dummy
  , ipv6table :: IPRTable IPv6 Bool -- Bool is dummy
  } deriving (Eq,Show)

----------------------------------------------------------------

ipMatch :: IP -> IPTable -> Bool
ipMatch (IPv4 ip) tbl = fromMaybe False $ T.lookup ir (ipv4table tbl)
  where
    ir = makeAddrRange ip 32
ipMatch (IPv6 ip) tbl = fromMaybe False $ T.lookup ir (ipv6table tbl)
  where
    ir = makeAddrRange ip 128

----------------------------------------------------------------

makeIPTable :: [IPRange] -> IPTable
makeIPTable is = IPTable {
    ipv4table = T.fromList $ zip ipv4addrRange (repeat True)
  , ipv6table = T.fromList $ zip ipv6addrRange (repeat True)
  }
  where
    ipv4addrRange = map toIPv4AddrRange $ filter isIPv4Range is
    ipv6addrRange = map toIPv6AddrRange $ filter isIPv6Range is

isIPv4Range :: IPRange -> Bool
isIPv4Range (IPv4Range _) = True
isIPv4Range _             = False

toIPv4AddrRange :: IPRange -> AddrRange IPv4
toIPv4AddrRange (IPv4Range x) = x
toIPv4AddrRange _             = error "toIPv4AddrRange"

isIPv6Range :: IPRange -> Bool
isIPv6Range (IPv6Range _) = True
isIPv6Range _             = False

toIPv6AddrRange :: IPRange -> AddrRange IPv6
toIPv6AddrRange (IPv6Range x) = x
toIPv6AddrRange _             = error "toIPv6AddrRange"