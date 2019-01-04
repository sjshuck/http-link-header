{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

-- | This module exports all the things at the same time.
module Network.HTTP.Link (
  module Network.HTTP.Link.Types
, module Network.HTTP.Link.Writer
, module Network.HTTP.Link.Parser
) where

import           Data.Text.Encoding
import safe      Network.HTTP.Link.Parser
import safe      Network.HTTP.Link.Types
import safe      Network.HTTP.Link.Writer
import           Web.HttpApiData

instance ToHttpApiData [Link] where
  toUrlPiece = toUrlPiece . writeLinkHeader
  toHeader = encodeUtf8 . writeLinkHeader

instance ToHttpApiData Link where
  toUrlPiece = toUrlPiece . writeLink
  toHeader = encodeUtf8 . writeLink
