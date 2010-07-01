{-# LANGUAGE OverloadedStrings #-}

module Milter.Switch (milter) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.IORef
import MailSpec
import Milter.Base
import Milter.Env
import Milter.Log
import Milter.Types
import Network.DomainAuth
import RPF
import System.IO

----------------------------------------------------------------

milter :: Env -> Handle -> IORef State -> IO ()
milter env hdl ref = withValidHandleDo $
    flip catch errorHandle $ do
      rpkt <- getPacket hdl
      switch env hdl ref rpkt
      milter env hdl ref
  where
    errorHandle e = logDebug env ref $ show e
    withValidHandleDo block = do
        closed <- hIsClosed hdl
        eof <- hIsEOF hdl
        unless (eof || closed) block

switch :: Env -> Handle -> IORef State -> Packet -> IO ()
switch env hdl ref (Packet 'O' bs) = open env hdl ref bs
switch env hdl ref (Packet 'C' bs) = conn env hdl ref bs
switch env hdl ref (Packet 'H' bs) = helo env hdl ref bs
switch env hdl ref (Packet 'M' bs) = mfro env hdl ref bs
switch _   hdl _   (Packet 'R' _ ) = continue hdl
switch _   hdl _   (Packet 'A' _ ) = continue hdl -- xxx
switch env hdl ref (Packet 'L' bs) = hedr env hdl ref bs
switch env hdl ref (Packet 'N' bs) = eohd env hdl ref bs
switch env hdl ref (Packet 'B' bs) = body env hdl ref bs
switch env hdl ref (Packet 'E' bs) = eoms env hdl ref bs
switch _   _   _   (Packet 'D'  _) = return ()
switch _   _   _   (Packet 'Q'  _) = return ()
switch env _   ref (Packet x    _) = logError env ref $ "Switch: " ++ [x]

----------------------------------------------------------------

mfilter :: Env -> Handle -> IORef State -> MailSpec -> BlockName -> IO ()
mfilter env hdl ref ms bn =
    let ply = policy env
    in case evalRPF ms ply bn of
         (l,A_Accept)   -> doit accept  "Accepted"  l
         (l,A_Discard)  -> doit discard "Discarded" l
         (l,A_Hold)     -> doit hold    "Held"      l
         (l,A_Reject)   -> doit reject  "Rejected"  l
         (_,A_Continue) -> if bn == B_Body
                           then logResult env ref "Accepted by default" >> continue hdl
                           else logMonitor env ref "continue" >> continue hdl
  where
    doit act m l = do
        logResult env ref $ m ++ " in line " ++ show l
        if logonly env
           then accept hdl
           else act hdl

----------------------------------------------------------------

type Filter = Env -> Handle -> IORef State -> ByteString -> IO ()

----------------------------------------------------------------

open :: Filter
open _ hdl _ _ = negoticate hdl

----------------------------------------------------------------

conn :: Filter
conn env hdl ref bs = do
    logResult env ref "SMTP connected"
    st <- readIORef ref
    let ip = getIP bs
        ms = (mailspec st) { msPeerIP = ip }
    writeIORef ref st { mailspec = ms }
    -- after IP set
    logDebug env ref $ '\t' : show ms
    mfilter env hdl ref ms B_Connect

----------------------------------------------------------------

helo :: Filter
helo env hdl ref _ = do
  logMonitor env ref "HELO"
  continue hdl

----------------------------------------------------------------

mfro :: Filter
mfro env hdl ref bs = do
    logMonitor env ref "MAIL FROM"
    let jmailfrom = extractDomain bs
    case jmailfrom of
      Nothing -> continue hdl -- xxx
      Just dom  -> do
          st <- readIORef ref
          xspf <- getSPF dom st
          let ms = (mailspec st) { msSPFResult = xspf, msMailFrom = jmailfrom }
          writeIORef ref st { mailspec = ms }
          logDebug env ref $ '\t' : show ms
          mfilter env hdl ref ms B_MailFrom
  where
    getSPF dom st = do
        let ip = msPeerIP (mailspec st)
        spf env dom ip

----------------------------------------------------------------

hedr :: Filter
hedr env hdl ref bs = do
    logMonitor env ref "DATA HEADER FIELD"
    st <- readIORef ref
    let (key,val) = getKeyVal bs
        ckey = canonicalizeKey key
        xm = pushField key val (xmail st)
        prd = pushPRD key val (prdspec st)
        (pv,ms) = checkField ckey val (parsedv st) (mailspec st)
    writeIORef ref $ st { xmail = xm, mailspec = ms, prdspec = prd, parsedv = pv}
    logDebug env ref $ "\t" ++ show xm ++ "\n\t" ++ show ms ++ "\n\t" ++ show prd
    continue hdl
  where
    checkField ckey val pv ms
      | ckey == dkFieldKey   = case parseDK val of
          Nothing -> (pv, ms)
          x@(Just pdk) -> (pv { mpdk = x},
                           ms { msSigDK = True
                              , msDKFrom = Just (dkDomain pdk) })
      | ckey == dkimFieldKey = case parseDKIM val of
          Nothing -> (pv, ms)
          x@(Just pdkim) -> (pv { mpdkim = x},
                             ms { msSigDKIM = True
                                , msDKIMFrom = Just (dkimDomain pdkim) })
      | otherwise = (pv, ms)

----------------------------------------------------------------

eohd :: Filter
eohd env hdl ref _ = do
    logMonitor env ref "DATA HEADER END"
    st <- readIORef ref
    let jfrom = getFrom st
        jprd  = decidePRD (prdspec st)
    sid <- getSenderID jprd
    let ms = (mailspec st) { msSenderIDResult = sid, msFrom = jfrom, msPRA = jprd }
    writeIORef ref st { mailspec = ms }
    mfilter env hdl ref ms B_Header
  where
    getFrom = decideFrom . prdspec
    getSenderID jprd =
        case jprd of
          Nothing  -> return DAPermError
          Just dom -> do
              ms <- mailspec <$> readIORef ref
              if jprd == msMailFrom ms
                 then return $ msSPFResult ms
                 else let ip = msPeerIP ms
                      in spf env dom ip

----------------------------------------------------------------

body :: Filter
body env hdl ref bs = do
    logMonitor env ref "DATA BODY"
    st <- readIORef ref
    let bc = getBody bs
        xm = pushBody bc (xmail st)
    writeIORef ref st { xmail = xm }
    continue hdl

----------------------------------------------------------------

eoms :: Filter
eoms env hdl ref _ = do
  logMonitor env ref "DATA BODY END"
  st <- readIORef ref
  let mail = finalizeMail (xmail st)
      mdk = mpdk (parsedv st)
      mdkim = mpdkim (parsedv st)
  xdk <- maybe (return DANone) (dk env mail) mdk
  xdkim <- maybe (return DANone) (dkim env mail) mdkim
  let ms = (mailspec st) { msDKResult = xdk, msDKIMResult = xdkim }
  mfilter env hdl ref ms B_Body
