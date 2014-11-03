{-# LANGUAGE OverloadedStrings, Trustworthy #-}

module Network.HTTP.Link.Writer (
  writeLink
, writeLinkHeader
, Link()
) where

import           Data.Text hiding (map)
import           Data.Monoid (mconcat)
import           Network.URI
import           Network.HTTP.Link.Types

writeParam :: (LinkParam, Text) -> Text
writeParam (t, v) = mconcat ["; ", pack $ show t, "=\"", v, "\""]

writeLink :: Link -> Text
writeLink l = mconcat $ ["<", esc $ href l, ">"] ++ map writeParam (linkParams l)
  where esc = pack . escapeURIString isAllowedInURI . unpack

writeLinkHeader :: [Link] -> Text
writeLinkHeader = intercalate ", " . map writeLink
