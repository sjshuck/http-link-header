# Changelog

## v1.2.0
* Removed `GLink` and associated functions, and upgraded old types and functions
  to be polymorphic

## v1.1.0
* Introduced `IsURI` typeclass, `GLink` type, and associated functions that
  allow polymorphic representations of links

## v1.0.3
* Added `ToHttpApiData` (from the `http-api-data` package) instances to enable
  embedding Link headers in URLs

## v1.0.2
* Added orphan `instance IsString URI` to simplify writing `Link` literals

## v1.0.1
* Added functions that work on `ByteString`

## v0.2.0
* Added Code of Conduct
* Changed license from Apache 2.0 to Unlicense
* Fixed `Title'`
* Changed link representation from `Text` to `URI`

## v0.1.0
* Initial release
