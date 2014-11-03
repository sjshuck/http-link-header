{-# LANGUAGE OverloadedStrings #-}

module ParserBench (benchmarks) where

import           Criterion
import           Network.HTTP.Link.Parser

benchmarks :: [Benchmark]
benchmarks = [
    bench "minimal" $ whnf parseLinkHeader "<http://example.com>; rel=\"next\""
  , bench "large" $ whnf parseLinkHeader "\n\t <  http://example.com>; rel=next; title=\"Hello world\",  <ftp://hello.world>; rev=license; someWeirdParam=\"YOLO LOL\", <https://long.ass.domain.name.just.because/lol>; rel=\"something something something http://some.thing/lol/rel\" "
  ]
