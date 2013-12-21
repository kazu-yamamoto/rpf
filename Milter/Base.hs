{-# LANGUAGE OverloadedStrings #-}

module Milter.Base (
    Packet (..)
  , getPacket
  , getIP
  , getKeyVal
  , getBody
  , negotiate
  , accept, discard, hold, reject, continue
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as X (unpack)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IP
import Data.List (foldl')
import Data.Monoid
import System.IO

----------------------------------------------------------------

accept :: Handle -> IO ()
accept hdl   = safePutPacket hdl $ Packet 'a' ""

discard :: Handle -> IO ()
discard hdl  = safePutPacket hdl $ Packet 'd' ""

hold :: Handle -> IO ()
hold hdl     = safePutPacket hdl $ Packet 't' ""

reject :: Handle -> IO ()
reject hdl   = safePutPacket hdl $ Packet 'r' ""

continue :: Handle -> IO ()
continue hdl = safePutPacket hdl $ Packet 'c' ""

----------------------------------------------------------------

data Packet = Packet Char ByteString

getPacket :: Handle -> IO Packet
getPacket hdl = do
    n <- fourBytesToInt <$> getNByte hdl 4
    Packet <$> getCmd hdl <*> getNByte hdl (n - 1)

putPacket :: Handle -> Packet -> IO ()
putPacket hdl (Packet c bs) = do
    let len = BS.length bs + 1
        pkt = intToFourBytes len <> fromChar c <> fromByteString bs
    BS.hPut hdl $ toByteString pkt

safePutPacket :: Handle -> Packet -> IO ()
safePutPacket hdl pkt = withOpenedHandleDo hdl $ putPacket hdl pkt

withOpenedHandleDo :: Handle -> IO () -> IO ()
withOpenedHandleDo hdl block = do
  closed <- hIsClosed hdl
  unless closed block

----------------------------------------------------------------

getKeyVal :: ByteString -> (ByteString, ByteString)
getKeyVal bs = (key,val)
  where
    kv = BS.split '\0' bs
    key = kv !! 0
    val = kv !! 1

----------------------------------------------------------------

getBody :: ByteString -> ByteString
getBody = BS.init -- removing the last '\0'

----------------------------------------------------------------

getIP :: ByteString -> IP
getIP bs
  | fam == '4' = IPv4 . read $ adr
  | otherwise  = IPv6 . read $ adr
  where
    ip  = BS.split '\0' bs !! 1
    fam = BS.head ip
    adr = BS.unpack $ BS.drop 3 ip

----------------------------------------------------------------

negotiate :: Handle -> IO ()
negotiate hdl =  putPacket hdl negoPkt -- do NOT use safePutPacket

negoPkt :: Packet
negoPkt = Packet 'O' $ toByteString $ ver <> act <> pro
  where
    ver = intToFourBytes 2 -- Sendmail 8.13.8, sigh
    act = intToFourBytes 0
    pro = intToFourBytes noRcpt

noRcpt :: Int
noRcpt    = 0x8
{- version 2 does not support, sigh
noUnknown = 0x100
noData    = 0x200
-}

----------------------------------------------------------------

getNByte :: Handle -> Int -> IO ByteString
getNByte = BS.hGet

getCmd :: Handle -> IO Char
getCmd hdl = BS.head <$> BS.hGet hdl 1

fourBytesToInt :: ByteString -> Int
fourBytesToInt = foldl' (\a b -> a * 256 + b) 0 . map fromIntegral . X.unpack

intToFourBytes :: Int -> Builder
intToFourBytes = fromInt32be . fromIntegral

{-
moddiv :: Int -> [Int]
moddiv q0 = [r4,r3,r2,r1]
  where
    (q1,r1) = q0 `divMod` 256
    (q2,r2) = q1 `divMod` 256
    (q3,r3) = q2 `divMod` 256
    r4 = q3 `mod` 256
-}
