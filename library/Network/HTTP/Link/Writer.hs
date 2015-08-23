{-# LANGUAGE OverloadedStrings, UnicodeSyntax, Safe, CPP #-}

module Network.HTTP.Link.Writer (
  writeLink
, writeLinkHeader
) where

import           Data.Text hiding (map)
#if __GLASGOW_HASKELL__ < 709
import           Data.Monoid (mconcat)
#endif
import           Network.URI
import           Network.HTTP.Link.Types

writeParamKey ∷ LinkParam → Text
writeParamKey Rel = "rel"
writeParamKey Anchor = "anchor"
writeParamKey Rev = "rev"
writeParamKey Hreflang = "hreflang"
writeParamKey Media = "media"
writeParamKey Title = "title"
writeParamKey Title' = "title*"
writeParamKey ContentType = "type"
writeParamKey (Other t) = t

writeParam ∷ (LinkParam, Text) → Text
writeParam (t, v) = mconcat ["; ", writeParamKey t, "=\"", escPar v, "\""]
  where escPar = pack . escapeURIString (/= '"') . unpack
        -- maybe URI escaping is not what we should do here? eh, whatever

writeLink ∷ Link → Text
writeLink l = mconcat $ ["<", pack . show $ href l, ">"] ++ map writeParam (linkParams l)

writeLinkHeader ∷ [Link] → Text
writeLinkHeader = intercalate ", " . map writeLink
