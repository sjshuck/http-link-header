{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Link.WriterSpec (module Network.HTTP.Link.WriterSpec) where

import           Test.Hspec
import           Network.HTTP.Link.Types
import           Network.HTTP.Link.Writer

spec :: Spec
spec = do
  describe "writeLinkHeader" $ do
    it "writes a single link" $ do
      writeLinkHeader [Link "http://example.com" [(Rel, "next")]]
        `shouldBe` "<http://example.com>; rel=\"next\""

    it "writes a single link with URL escaping" $ do
      writeLinkHeader [Link "http://example.com/hello world" [(Rel, "next")]]
        `shouldBe` "<http://example.com/hello%20world>; rel=\"next\""

    it "writes a single link with multiple parameters" $ do
      writeLinkHeader [Link "http://example.com" [(Rel, "next"), (Title, "hello world")]]
        `shouldBe` "<http://example.com>; rel=\"next\"; title=\"hello world\""

    it "writes a multiple links" $ do
      writeLinkHeader [ Link "http://example.com" [(Rel, "next"), (Title, "hello world")]
                      , Link "https://hello.world" [(Rev, "license")] ]
        `shouldBe` "<http://example.com>; rel=\"next\"; title=\"hello world\", <https://hello.world>; rev=\"license\""
