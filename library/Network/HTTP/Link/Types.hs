{-# LANGUAGE Safe #-}

-- | The data type definitions for the HTTP Link header.
module Network.HTTP.Link.Types (module Network.HTTP.Link.Types) where

import           Data.Text

-- | The link attribute key.
data LinkParam = Rel | Anchor | Rev | Hreflang | Media | Title | Title' | ContentType | Other Text
  deriving (Eq, Show)

-- | A single link.
data Link = Link Text [(LinkParam, Text)]
  deriving (Eq, Show)

-- | Extracts the URI from the link.
href :: Link -> Text
href (Link h _) = h

-- | Extracts the parameters from the link.
linkParams :: Link -> [(LinkParam, Text)]
linkParams (Link _ ps) = ps
