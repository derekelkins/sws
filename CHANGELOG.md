# Change Log
All notable changes to this project will be documented in this file.

## 0.4.0.0 - 2017-09-10
### Changed
- Upper bound bumps.
### Removed
- TLS certificate generation. The functionality was removed from the crypto library this depends upon.

## 0.3.1.2 - 2015-08-25
### Changed
- Upper bound bumps.
- Make STUN server look up a bit more robust.

## 0.3.1.1 - 2015-04-17
### Changed
- Upper bound bump to support GHC 7.10.

## 0.3.1.0 - 2015-01-07
### Added
- Added Public mode.

### Changed
- Dependency bounds changed on warp-tls to pull in critical bug fix.

## 0.3.0.0 - 2014-12-29
### Added
- CHANGELOG.md and README.md
- Support for generating throw-away TLS certificates.
- Support for generating password for Basic Authentication.
- Support for adding custom headers to all responses.
- Support getting public IP via Google STUN server.
- Added Quiet mode and Dev Mode.

### Changed
- Change defaults to use HTTPS and Basic Authentication.
- Change display of connection information to a copyable address.
- Currently works around a bug in x509 to generate valid certificates.  (Pull request outstanding.)

### Removed
- \-\-allow\-http is removed.  It did not appear to work anyway.

## 0.2.0.0 - 2014-12-06
### Added
- Support for uploading files.  This includes overwriting existing files.

## 0.1.0.0 - 2014-11-11
### Added
- This is intended to be fully usable for it's intended purpose, namely statically serving
a directory tree.
