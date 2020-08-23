{-# LANGUAGE UnicodeSyntax, Safe #-}

-- | The data type definitions for the HTTP Link header.
module Network.HTTP.Link.Types where

import           Data.Text
import           Network.URI

-- | The link attribute key.
data LinkParam = Rel | Anchor | Rev | Hreflang | Media | Title | Title' | ContentType | Other Text
  deriving (Eq, Show)

-- | A single link containing some representation of a URL.
--
-- @since 1.1.0
data GLink uri = Link uri [(LinkParam, Text)]
    deriving (Eq, Show)

-- | Types that can represent URLs.
--
-- For example, to parse links containing 'Text.URI.URI' from the modern-uri
-- package, simply define:
--
-- @
--    instance IsURI Modern.URI where
--        uriFromText = left displayException . mkURI
-- @
--
-- @since 1.1.0
class IsURI uri where
    uriFromText ∷ Text → Either String uri

instance IsURI URI where
    uriFromText = maybe (Left "") Right . parseURIReference . unpack

instance IsURI Text where
    uriFromText = Right

-- | A single link containing a network-uri URI.  Most of this library is
-- specialized to this case.
type Link = GLink URI

-- | Extracts the URI from the link.
href ∷ Link → URI
href (Link h _) = h

-- | Extracts the parameters from the link.
linkParams ∷ Link → [(LinkParam, Text)]
linkParams (Link _ ps) = ps

lnk ∷ String → [(LinkParam, Text)] → Maybe Link
lnk u r = parseURI u >>= return . \x → Link x r
