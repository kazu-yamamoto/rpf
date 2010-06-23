module RPF.Lexer where

import Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as P
import RPF.Types

----------------------------------------------------------------

type Lexer st a = GenParser Char st a

----------------------------------------------------------------

rpfStyle :: LanguageDef st
rpfStyle = emptyDef {
    commentStart   = "/*"
  , commentEnd     = "*/"
  , commentLine    = "//"
  , nestedComments = True
  , identStart     = char '$'
  , identLetter    = alphaNum <|> oneOf "_-"
  , reservedOpNames= ["&&", "==", "!=", "="]
  , reservedNames  = actionWords ++ variableNames
                  ++ resultWords ++ blockNames ++ noyes
  , caseSensitive  = True
  }

lexer :: P.TokenParser a
lexer  = P.makeTokenParser rpfStyle

----------------------------------------------------------------

whiteSpace :: Lexer st ()
whiteSpace = P.whiteSpace lexer

----------------------------------------------------------------

semi :: Lexer st String
semi       = P.semi lexer

comma :: Lexer st String
comma      = P.comma lexer

colon :: Lexer st String
colon      = P.colon lexer

identifier :: Lexer st String
identifier = P.identifier lexer

----------------------------------------------------------------

symbol :: String -> Lexer st String
symbol     = P.symbol lexer

reserved :: String -> Lexer st ()
reserved   = P.reserved lexer

reservedOp :: String -> Lexer st ()
reservedOp = P.reservedOp lexer

----------------------------------------------------------------

braces :: Lexer st a -> Lexer st a
braces     = P.braces lexer

lexeme :: Lexer st a -> Lexer st a
lexeme     = P.lexeme lexer

commaSep1 :: Lexer st a -> Lexer st [a]
commaSep1  = P.commaSep1 lexer
