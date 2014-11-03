{-# LANGUAGE OverloadedStrings #-}

module WriterBench (benchmarks) where

import           Criterion
import           Network.HTTP.Link.Types
import           Network.HTTP.Link.Writer

benchmarks :: [Benchmark]
benchmarks = [
    bench "minimal" $ whnf writeLinkHeader [Link "http://example.com/thing" [(Rel, "next")]]
  , bench "large" $ whnf writeLinkHeader [ Link "http://example.com/something long" [ (Rel, "next prev http://hello.world/undefined")
                                                                                    , (Title, "this is a test benchmark thingy")]
                                         , Link "https://use.tls.everywhere.pls" [ (Rel, "license")
                                                                                 , (Rev, "author") ]]
  ]
