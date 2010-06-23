module RPF.Parser where

import Control.Monad
import Data.IP
import Data.List
import Data.Maybe
import Network.DNS.Types (Domain)
import Network.DomainAuth
import Parsec hiding (Parser)
import RPF.Domain
import RPF.IP
import RPF.Lexer
import RPF.State
import RPF.Types
import Text.ParserCombinators.Parsec.Expr

{-
import Debug.Trace
infixr 0 .$.
(.$.) :: Show a => (a -> b) -> a -> b
f .$. x = trace (show x) f x
-}

----------------------------------------------------------------

type Parser a = Lexer ParserState a

----------------------------------------------------------------

parsePolicy :: String -> Policy
parsePolicy cs = case runParser config initialState "" cs of
                   Left  err -> error $ show err
                   Right rs  -> rs

----------------------------------------------------------------

config :: Parser Policy
config = do
    whiteSpace
    blks <- many1 defBlock
    eof
    st <- getState
    checkUnused (unused st)
    return $ Policy blks (iptbls st) (domtbls st)
  where
    iptbls = map makeIPTable . reverse . iplol
    domtbls = map makeDomainTable . reverse . domlol
    checkUnused [] = return ()
    checkUnused us = unexpected $ ": Not used -- " ++ concat (intersperse ", " us)

----------------------------------------------------------------

defBlock :: Parser Block
defBlock = many definition >> block

definition :: Parser ()
definition = do
    cst <- identifier -- $foo
    reservedOp "="
    dat <- ipList <|> domainList <|> resultList
    semi
    define cst dat
    return ()
  where
    define cst dat = do
        st <- getState
        let ent = lookup cst (consttbl st)
        when (isJust ent) $ unexpected $ ": Multiple declarations of '" ++ cst ++ "'"
        setState st {
            consttbl = (cst, dat) : consttbl st
          , unused = cst : unused st
          }

block :: Parser Block
block = do
    blknm <- blockname
    checkBlock blknm
    cs <- braces (many actionCond)
    check blknm cs
    nextBlock
    return $ Block blknm cs
  where
    blockname :: Parser BlockName
    blockname = sym2enum blockNames [minBound..maxBound]
    checkBlock blknm = do
        st <- getState
        let cblknm = head $ blocks st
        when (blknm /= cblknm) $ unexpected $ ": " ++ show cblknm ++ "{} is expected"
    nextBlock = do st <- getState
                   setState st { blocks = tail $ blocks st }
    check blknm cs = case last cs of
      ActionCond _ Nothing _ -> return ()
      _                      -> unexpected $ ": action without condition must exist in " ++ show blknm ++ "{}"

----------------------------------------------------------------

actionCond :: Parser ActionCond
actionCond = do
    pos <- getPosition
    let l = sourceLine pos
    act <- action
    cnd <- option Nothing condition
    semi
    return $ ActionCond l cnd act
  where
    action :: Parser Action
    action = sym2enum actionWords [minBound..maxBound]
    condition = do
        colon
        cnd <- cond
        return $ Just cnd

----------------------------------------------------------------

cond :: Parser Cond
cond = buildExpressionParser table term
    where
      table = [[Infix opAnd AssocRight]]

term :: Parser Cond
term = do
    char '#'
    var@(vtyp,vid) <- variable
    checkVar vid
    opr <- opMatch <|> opNotMatch
    cst@(ctyp,cval) <- constant
    when (vtyp /= ctyp) $ unexpected ": Data type mismatch"
    when (vtyp == DT_Res) $ checkResult vid cval
    return $ var `opr` cst
  where
    variable :: Parser Variable
    variable = var2enum variableNames [minBound..maxBound] variableTypes
    checkVar vid = do
        st <- getState
        let blknm = head $ blocks st
            sanity = fromJust $ lookup blknm varSanity
        unless (vid `elem` sanity) $ unexpected $ ": #" ++ show vid ++ " cannot be used in " ++ show blknm ++ "{}"
    checkResult vid (CV_Result cval) = do
        let sanity = fromJust $ lookup vid resultSanity
        mapM_ (test vid sanity) cval
    checkResult _ _ = return ()
    test vid rs r = unless (r `elem` rs) $ unexpected $ ": '" ++ show r ++ "' cannot be used for #" ++ show vid

----------------------------------------------------------------

constant :: Parser Constant
constant = ipList <|> domainList <|> resultList2
       <|> definedConstant <|> yesOrNo

definedConstant :: Parser Constant
definedConstant = identifier >>= resolve
  where
    resolve cst = do
        st <- getState
        let ent = lookup cst $ consttbl st
        when (isNothing ent) $ unexpected $ ": No definition of '" ++ cst ++ "'"
        let cused = cst : used st
            cunused = delete cst $ unused st
        setState st { used = cused, unused = cunused }
        let cd@(typ, CV_Index n) = fromJust ent
        if typ == DT_Res then return (typ, CV_Result (reslol st !! n))
                         else return cd

yesOrNo :: Parser Constant
yesOrNo = do
    b <- yesno
    return (DT_Sig, CV_Sig b)
  where
    yesno :: Parser Bool
    yesno = sym2enum noyes [minBound..maxBound]

----------------------------------------------------------------

op :: String -> a -> Parser a
op str opr = do reservedOp str
                return opr
opAnd :: Parser (Cond -> Cond -> Cond)
opAnd      = op "&&" (:&&)

opMatch :: Parser (Variable -> Constant -> Cond)
opMatch    = op "==" (:==)

opNotMatch :: Parser (Variable -> Constant -> Cond)
opNotMatch = op "!=" (:!=)

----------------------------------------------------------------

ip4range :: Parser IPRange
ip4range = IPv4Range . read <$> many1 (oneOf $ ['0'..'9'] ++ "./")
ip6range :: Parser IPRange
ip6range = IPv6Range . read <$> many1 (oneOf $ ['0'..'9'] ++ ['a'..'f']  ++ ['A'..'F'] ++ ".:/")

ipList :: Parser Constant
ipList = do
    ips <- commaSep1 (try(lexeme ip4range) <|> lexeme ip6range)
    n <- index ips
    return (DT_IP, CV_Index n)
  where
    index ips = do
        st <- getState
        let n = ipcnt st
        setState st {
            iplol = ips : iplol st
          , ipcnt = n + 1
          }
        return n

----------------------------------------------------------------

domain :: Parser Domain
domain = (char '"' >> many1 (noneOf "\"")) <* symbol "\""

domainList :: Parser Constant
domainList = do
    dms <- commaSep1 (lexeme domain)
    n <- index dms
    return (DT_Dom, CV_Index n)
  where
    index dms = do
        st <- getState
        let n = domcnt st
        setState st {
            domlol = dms : domlol st
          , domcnt = n + 1
          }
        return n

----------------------------------------------------------------

result :: Parser DAResult
result = (char '\'' >> authResult) <* char '\''
  where
    authResult :: Parser DAResult
    authResult = sym2enum resultWords [minBound..maxBound]

resultList :: Parser Constant
resultList = do
    rs <- commaSep1 (lexeme result)
    n <- index rs
    return (DT_Res, CV_Index n)
  where
    index rs = do
        st <- getState
        let n = rescnt st
        setState st {
            reslol = rs : reslol st
          , rescnt = n + 1
          }
        return n

resultList2 :: Parser Constant
resultList2 = do
  rs <- commaSep1 (lexeme result)
  return (DT_Res, CV_Result rs)

----------------------------------------------------------------

sym2enum :: [String] -> [a] -> Parser a
sym2enum ss es = choice ps
  where
    func res sym = do {reserved res; return sym}
    ps = zipWith func ss es

var2enum :: [String] -> [VariableId] -> [DataType] -> Parser Variable
var2enum ss ns ts = choice ps
  where
    func res nam typ = do {reserved res; return (typ,nam)}
    ps = zipWith3 func ss ns ts
