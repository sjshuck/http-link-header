{-# LANGUAGE OverloadedStrings, UnicodeSyntax, Safe, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | The parser for the HTTP Link header as defined in RFC 5988.
-- More liberal than the RFC though:
-- does not validate URLs and other deep stuff,
-- accepts whitespace in weird places.
module Network.HTTP.Link.Parser (
  linkHeader
, parseLinkHeader'
, parseLinkHeader
, parseLinkHeaderBS'
, parseLinkHeaderBS
  -- ** Generic links ('GLink')
  --
  -- | These functions are like the ones above, except they represent URLs as
  -- @('IsURI' uri) ⇒ uri@ values rather than concrete 'URI's from the
  -- network-uri package.  Use these if you need access to the raw URLs
  -- (@uri ~ Text@) or some other representation of URLs.
  --
  -- @since 1.1.0
, parseGLinkHeader'
, parseGLinkHeader
, parseGLinkHeaderBS'
, parseGLinkHeaderBS
) where

import           Prelude hiding (takeWhile, take)
import           Control.Applicative
import           Control.Error.Util (hush)
import           Data.Text hiding (takeWhile, map, take)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (mconcat)
#endif
import           Data.Attoparsec.Text
import           Network.URI
import           Network.HTTP.Link.Types

allConditions ∷ [a → Bool] → a → Bool
allConditions cs x = and $ map ($ x) cs

charWS ∷ Char → Parser ()
charWS x = skipSpace >> char x >> skipSpace

quotedString ∷ Parser Text
quotedString = do
  char '"'
  v ← many stringPart
  char '"'
  return $ pack $ unEscapeString $ unpack $ mconcat v
  where stringPart = takeWhile1 (allConditions [(/= '"'), (/= '\\')]) <|> escapedChar
        escapedChar = char '\\' >> take 1

paramName ∷ Text → LinkParam
paramName "rel"       = Rel
paramName "anchor"    = Anchor
paramName "rev"       = Rev
paramName "hreflang"  = Hreflang
paramName "media"     = Media
paramName "title"     = Title
paramName "title*"    = Title'
paramName "type"      = ContentType
paramName x           = Other x

relType ∷ Parser Text
relType = takeWhile1 $ inClass "-0-9a-z."

paramValue ∷ LinkParam → Parser Text
paramValue Rel    = quotedString <|> relType
paramValue Rev    = quotedString <|> relType
paramValue Title' = takeWhile (allConditions [not . isSpace])
paramValue _      = quotedString

param ∷ Parser (LinkParam, Text)
param = do
  charWS ';'
  n ← takeWhile (allConditions [(/= '='), not . isSpace])
  let n' = paramName n
  charWS '='
  v ← paramValue n'
  return (n', v)

glink ∷ (IsURI uri) ⇒ Parser (GLink uri)
glink = do
  charWS '<'
  linkText ← takeWhile1 $ allConditions [(/= '>'), not . isSpace]
  charWS '>'
  params ← many' $ param
  skipSpace
  case uriFromText linkText of
    Right u → return $ Link u params
    Left e → fail $ "Couldn't parse the URI " ++ show linkText ++ if e == "" then "" else ": " ++ e

glinkHeader ∷ (IsURI uri) ⇒ Parser [GLink uri]
glinkHeader = glink `sepBy'` (char ',')

-- | The Attoparsec parser for the Link header.
linkHeader ∷ Parser [Link]
linkHeader = glinkHeader

-- | Parses a GLink header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseGLinkHeader' ∷ (IsURI uri) ⇒ Text → Either String [GLink uri]
parseGLinkHeader' = parseOnly glinkHeader

-- | Parses a GLink header, returns a Maybe.
parseGLinkHeader ∷ (IsURI uri) ⇒ Text → Maybe [GLink uri]
parseGLinkHeader = hush . parseGLinkHeader'

-- | Parses a GLink header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseGLinkHeaderBS' ∷ (IsURI uri) ⇒ ByteString → Either String [GLink uri]
parseGLinkHeaderBS' = parseGLinkHeader' . decodeUtf8

-- | Parses a GLink header, returns a Maybe.
parseGLinkHeaderBS ∷ (IsURI uri) ⇒ ByteString → Maybe [GLink uri]
parseGLinkHeaderBS = parseGLinkHeader . decodeUtf8

-- | Parses a Link header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseLinkHeader' ∷ Text → Either String [Link]
parseLinkHeader' = parseGLinkHeader'

-- | Parses a Link header, returns a Maybe.
parseLinkHeader ∷ Text → Maybe [Link]
parseLinkHeader = parseGLinkHeader

-- | Parses a Link header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseLinkHeaderBS' ∷ ByteString → Either String [Link]
parseLinkHeaderBS' = parseGLinkHeaderBS'

-- | Parses a Link header, returns a Maybe.
parseLinkHeaderBS ∷ ByteString → Maybe [Link]
parseLinkHeaderBS = parseGLinkHeaderBS
