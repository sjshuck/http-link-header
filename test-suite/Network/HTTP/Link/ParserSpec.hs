{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Link.ParserSpec (module Network.HTTP.Link.ParserSpec) where

import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Data.Text
import           Network.HTTP.Link.Types
import           Network.HTTP.Link.Parser

spec :: Spec
spec = do
  describe "linkHeader" $ do
    it "parses a single link" $ do
      ("<http://example.com>; rel=\"example\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "example")] ]

    it "ignores empty attributes" $ do
      ("<http://example.com>; title=\"\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [] ]

    it "parses a link with backslash escaped attributes" $ do
      ("<http://example.com>; title=\"some \\\" thing \\\"\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Title, "some \" thing \"")] ]

    it "parses a link with URI escaped attributes" $ do
      ("<http://example.com>; title=\"some %22 thing %22\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Title, "some \" thing \"")] ]

    it "parses a link with multiple attributes" $ do
      ("<http://example.com>; rel=\"example\"; title=\"example dot com\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "example"), (Title, "example dot com")] ]

    it "parses a link with unquoted rel, rev attributes" $ do
      ("<http://example.com>; rel=next; rev=prev" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "next"), (Rev, "prev")] ]

    it "parses a link with weird whitespace all over the place" $ do
      ("\n\t   < http://example.com\t>;rel=\t\"example\";   \ttitle =\"example dot com\" \n " :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "example"), (Title, "example dot com")] ]
