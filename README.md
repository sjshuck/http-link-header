# http-link-header [![Hackage](https://img.shields.io/hackage/v/http-link-header.svg?style=flat)](https://hackage.haskell.org/package/http-link-header) [![Build Status](https://img.shields.io/travis/myfreeweb/http-link-header.svg?style=flat)](https://travis-ci.org/myfreeweb/http-link-header) [![Coverage Status](https://img.shields.io/coveralls/myfreeweb/http-link-header.svg?style=flat)](https://coveralls.io/r/myfreeweb/http-link-header) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A Haskell library than implements a parser and a writer for the HTTP Link header as specified in [RFC 5988 "Web Linking"](https://tools.ietf.org/html/rfc5988).
Very fast.
Not 100% compliant (does anyone actually use `title*`?).
Liberal parser, conservative writer.

## Usage

```haskell
import Network.HTTP.Link
import Network.URI
import Data.Maybe

----- Writing
writeLinkHeader [ Link (fromJust $ parseURI "https://example.com/hello%20world") [(Rel, "next"), (Title, "hello world")]
                , Link (fromJust $ parseURI "https://yolo.tld") [(Rel, "license")] ]
-- "<https://example.com/hello%20world>; rel=\"next\"; title=\"hello world\", <https://yolo.tld>; rel=\"license\""

----- Parsing
parseLinkHeader "<https://example.com/2>; rel=\"next\", <https://example.com/0>; rel=prev"
-- Just [ Link https://example.com/2 [(Rel, "next")]
--      , Link https://example.com/0 [(Rel, "prev")] ]
```

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack bench

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!
Bugfixes and simple non-breaking improvements will be accepted without any questions :-)

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/2/0/).

[The list of contributors is available on GitHub](https://github.com/myfreeweb/http-link-header/graphs/contributors).

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
