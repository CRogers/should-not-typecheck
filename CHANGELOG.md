# `should-not-typecheck` changelog

## 2.1.0
* Support GHC 8.0.1 (see https://github.com/CRogers/should-not-typecheck/pull/6).

## 2.0.1
* Support HUnit 1.3

## 2.0
* Changed API to require `NFData a` so we can fully evaluate expressions, rather than just converting to WHNF.

## 1.0.1
* Use `throwIO` instead of `throw` for exception ordering safety.

## 1.0
* Stabilise API at 1.0 release.
* Allow building on 7.6.3.

## 0.1.0.0
* Initial version.
