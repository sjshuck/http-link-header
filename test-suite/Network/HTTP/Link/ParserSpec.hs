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

    it "parses empty attributes" $ do
      ("<http://example.com>; title=\"\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Title, "")] ]

    it "parses custom attributes" $ do
      ("<http://example.com>; weirdThingy=\"something\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Other "weirdThingy", "something")] ]

    it "parses backslash escaped attributes" $ do
      ("<http://example.com>; title=\"some \\\" thing \\\"\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Title, "some \" thing \"")] ]

    it "parses escaped attributes" $ do
      ("<http://example.com>; title=\"some %22 thing %22\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Title, "some \" thing \"")] ]

    it "parses multiple attributes" $ do
      ("<http://example.com>; rel=\"example\"; title=\"example dot com\"" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "example"), (Title, "example dot com")] ]

    it "parses custom attributes named similarly to standard ones" $ do
      -- this was caught by QuickCheck! <3
      ("<http://example.com>; rel=hello; relAtion=\"something\"; rev=next" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "hello"), (Other "relAtion", "something"), (Rev, "next")] ]

    it "parses unquoted rel, rev attributes" $ do
      ("<http://example.com>; rel=next; rev=prev" :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "next"), (Rev, "prev")] ]

    it "parses weird whitespace all over the place" $ do
      ("\n\t   < http://example.com\t>;rel=\t\"example\";   \ttitle =\"example dot com\" \n " :: Text) ~> linkHeader
        `shouldParse` [ Link "http://example.com" [(Rel, "example"), (Title, "example dot com")] ]
