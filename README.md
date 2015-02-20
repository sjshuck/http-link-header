# http-link-header [![Hackage](https://img.shields.io/hackage/v/http-link-header.svg?style=flat)](https://hackage.haskell.org/package/http-link-header) [![Build Status](https://img.shields.io/travis/myfreeweb/http-link-header.svg?style=flat)](https://travis-ci.org/myfreeweb/http-link-header) [![Coverage Status](https://img.shields.io/coveralls/myfreeweb/http-link-header.svg?style=flat)](https://coveralls.io/r/myfreeweb/http-link-header) [![ISC License](https://img.shields.io/badge/license-ISC-red.svg?style=flat)](https://tldrlegal.com/license/-isc-license)

A Haskell library than implements a parser and a writer for the HTTP Link header as specified in [RFC 5988 "Web Linking"](https://tools.ietf.org/html/rfc5988).
Very fast.
Not 100% compliant (does anyone actually use `title*`?).
Liberal parser, conservative writer.

## Usage

```haskell
import Network.HTTP.Link

----- Writing
writeLinkHeader [ Link "https://example.com/hello world" [(Rel, "next"), (Title, "hello world")]
                , Link "https://yolo.tld" [(Rel, "license")] ]
-- "<https://example.com/hello%20world>; rel=\"next\"; title=\"hello world\", <https://yolo.tld>; rel=\"license\""

----- Parsing
parseLinkHeader "<https://example.com/2>; rel=\"next\", <https://example.com/0>; rel=prev"
-- Just [ Link "https://example.com/2" [(Rel, "next")]
--      , Link "https://example.com/0" [(Rel, "prev")] ]
```

## Development

```bash
# Update to latest version of Cabal.
cabal update
cabal install cabal-install

# Initialize a sandbox and install the package's dependencies.
make install

# Configure & build the package.
make configure build

# Test package.
make test

# Benchmark package.
make bench

# Start a REPL.
make repl

# Generate documentation.
make haddock
```

## License

Copyright 2014-2015 Greg V <greg@unrelenting.technology>  
Available under the ISC license, see the `COPYING` file
