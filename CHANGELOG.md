## [_Unreleased_](https://github.com/freckle/hspec-expectations-json/compare/v1.0.2.1...main)

## [v1.0.3.0](https://github.com/freckle/hspec-expectations-json/compare/v1.0.2.0...v1.0.2.1)

- Fixed issue with `expandHeterogenousArrays` not working for nested Objects

## [v1.0.2.0](https://github.com/freckle/hspec-expectations-json/compare/v1.0.1.1...v1.0.2.0)

- Add `shouldBeJsonNormalized` and `Normalizer` to better support configurable matching
- Added new option for `treatNullsAsMissing` that will treat nulls fields as if they are the same as omitted ones when doing a comparison

## [v1.0.1.1](https://github.com/freckle/hspec-expectations-json/compare/v1.0.1.0...v1.0.1.1)

- Add invariant for all matchers for equality. (ex: forall a. a `shouldMatchJson` a)

## [v1.0.1.0](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.6...v1.0.1.0)

- Colorize diff in expectation failure, if connected to a terminal or running on
  GitHub Actions.
- Add a `.Lifted` module with `MonadIO` versions

## [v1.0.0.6](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.5...v1.0.0.6)

- Support GHC 9.0 and 9.2
- Re-add support for aeson-1.x

## [v1.0.0.5](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.4...v1.0.0.5)

- Add lower bound for `aeson` 2.x

## [v1.0.0.4](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.3...v1.0.0.4)

- Remove dependencies upper bounds

## [v1.0.0.3](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.2...v1.0.0.3)

- Less restrictive upper bound on `base`

## [v1.0.0.2](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.1...v1.0.0.2)

- Add explicit and relaxed `aeson` and `text` bounds

## [v1.0.0.1](https://github.com/freckle/hspec-expectations-json/compare/v1.0.0.0...v1.0.0.1)

- Relax `base` (GHC) upper bound

## [v1.0.0.0](https://github.com/freckle/hspec-expectations-json/tree/v1.0.0.0)

First tagged release.
