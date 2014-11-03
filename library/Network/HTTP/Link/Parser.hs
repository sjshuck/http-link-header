{-# LANGUAGE OverloadedStrings, Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | The parser for the HTTP Link header as defined in RFC 5988.
-- More liberal than the RFC though:
-- does not validate URLs and other deep stuff,
-- accepts whitespace in weird places.
module Network.HTTP.Link.Parser (
  linkHeader
, parseLinkHeader'
, parseLinkHeader
) where

import           Prelude hiding (takeWhile, take)
import           Data.Text hiding (takeWhile, map, take)
import           Data.Char (isSpace)
import           Data.Monoid (mconcat)
import           Data.Attoparsec.Text
import           Control.Applicative
import           Control.Error.Util (hush)
import           Network.URI
import           Network.HTTP.Link.Types

allConditions :: [a -> Bool] -> a -> Bool
allConditions cs x = and $ map ($ x) cs

charWS :: Char -> Parser ()
charWS x = skipSpace >> char x >> skipSpace

quotedString :: Parser Text
quotedString = do
  char '"'
  v <- many stringPart
  char '"'
  return $ pack $ unEscapeString $ unpack $ mconcat v
  where stringPart = takeWhile1 (allConditions [(/= '"'), (/= '\\')]) <|> escapedChar
        escapedChar = char '\\' >> take 1

paramName :: Text -> LinkParam
paramName "rel"       = Rel
paramName "anchor"    = Anchor
paramName "rev"       = Rev
paramName "hreflang"  = Hreflang
paramName "media"     = Media
paramName "title"     = Title
paramName "title*"    = Title'
paramName "type"      = ContentType
paramName x           = Other x

relType :: Parser Text
relType = takeWhile1 $ inClass "-0-9a-z."

paramValue :: LinkParam -> Parser Text
paramValue Rel = quotedString <|> relType
paramValue Rev = quotedString <|> relType
paramValue _ = quotedString

param :: Parser (LinkParam, Text)
param = do
  charWS ';'
  n <- takeWhile (allConditions [(/= '='), not . isSpace])
  let n' = paramName n
  charWS '='
  v <- paramValue n'
  return (n', v)

link :: Parser Link
link = do
  charWS '<'
  url <- takeWhile1 $ allConditions [(/= '>'), not . isSpace]
  charWS '>'
  params <- many' $ param
  skipSpace
  return $ Link url params

-- | The Attoparsec parser for the Link header.
linkHeader :: Parser [Link]
linkHeader = link `sepBy'` (char ',')

-- | Parses a Link header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseLinkHeader' :: Text -> Either String [Link]
parseLinkHeader' = parseOnly linkHeader

-- | Parses a Link header, returns a Maybe.
parseLinkHeader :: Text -> Maybe [Link]
parseLinkHeader = hush . parseLinkHeader'
