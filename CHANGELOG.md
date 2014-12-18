# Change Log
All notable changes to this project will be documented in this file.

## 0.3.0.0 - Unreleased
### Added
- CHANGELOG.md and README.md
- Support for generating throw-away TLS certificates.
- Support for generating password for Basic Authentication.
- Support for adding custom headers to all responses.

### Changed
- Change defaults to use HTTPS and Basic Authentication.
- Requires a currently non-existent version of x509 to generate valid certificates.  (Pull request outstanding.)

### Removed
- \-\-allow\-http is removed.  It did not appear to work anyway.

## 0.2.0.0 - 2014-12-06
### Added
- Support for uploading files.  This includes overwriting existing files.

## 0.1.0.0 - 2014-11-11
### Added
- This is intended to be fully usable for it's intended purpose, namely statically serving
a directory tree.
