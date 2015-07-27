{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, FlexibleInstances #-}

-- | This module exports all the things at the same time.
module Network.HTTP.Link (
  module Network.HTTP.Link.Types
, module Network.HTTP.Link.Writer
, module Network.HTTP.Link.Parser
) where

import           Data.ByteString.Conversion
import safe      Network.HTTP.Link.Types
import safe      Network.HTTP.Link.Writer
import safe      Network.HTTP.Link.Parser

instance ToByteString [Link] where
  builder = builder . writeLinkHeader

instance ToByteString Link where
  builder = builder . writeLink
