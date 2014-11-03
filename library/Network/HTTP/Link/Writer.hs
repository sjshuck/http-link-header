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
writeParam (t, v) = mconcat ["; ", pack $ show t, "=\"", escPar v, "\""]
  where escPar = pack . escapeURIString (/= '"') . unpack
        -- maybe URI escaping is not what we should do here? eh, whatever

writeLink :: Link -> Text
writeLink l = mconcat $ ["<", escURI $ href l, ">"] ++ map writeParam (linkParams l)
  where escURI = pack . escapeURIString isAllowedInURI . unpack

writeLinkHeader :: [Link] -> Text
writeLinkHeader = intercalate ", " . map writeLink
