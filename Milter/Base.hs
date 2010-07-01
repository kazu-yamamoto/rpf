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

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L hiding (ByteString)
import Data.Char
import Data.IP
import Data.List (foldl')
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
    let len = fromIntegral (L.length bs) + 1
    L.hPut hdl $ intToFourBytes len
    L.hPut hdl $ c `L.cons` bs

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
    kv = L.split '\0' bs
    key = kv !! 0
    val = kv !! 1

----------------------------------------------------------------

getBody :: ByteString -> ByteString
getBody = L.init -- removing the last '\0'

----------------------------------------------------------------

getIP :: ByteString -> IP
getIP bs
  | fam == '4' = IPv4 . read $ adr
  | otherwise  = IPv6 . read $ adr
  where
    ip  = L.split '\0' bs !! 1
    fam = L.head ip
    adr = L.unpack $ L.drop 3 ip

----------------------------------------------------------------

negotiate :: Handle -> IO ()
negotiate hdl =  putPacket hdl negoPkt -- do NOT use safePutPacket

negoPkt :: Packet
negoPkt = Packet 'O' $ ver +++ act +++ pro
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
getNByte = L.hGet

getCmd :: Handle -> IO Char
getCmd hdl = L.head <$> L.hGet hdl 1

fourBytesToInt :: ByteString -> Int
fourBytesToInt = foldl' (\a b -> a * 256 + b) 0 . map ord . L.unpack

intToFourBytes :: Int -> ByteString
intToFourBytes = L.pack. reverse . map chr . moddiv 4

moddiv :: Int -> Int -> [Int]
moddiv 0 _ = []
moddiv i m = (m `mod` 256) : moddiv (i - 1) (m `div` 256)

----------------------------------------------------------------

(+++) :: ByteString -> ByteString -> ByteString
(+++) = L.append
