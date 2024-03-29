# Hspec Expectations for JSON Values

[![Hackage](https://img.shields.io/hackage/v/hspec-expectations-json.svg?style=flat)](https://hackage.haskell.org/package/hspec-expectations-json)
[![Stackage Nightly](http://stackage.org/package/hspec-expectations-json/badge/nightly)](http://stackage.org/nightly/package/hspec-expectations-json)
[![Stackage LTS](http://stackage.org/package/hspec-expectations-json/badge/lts)](http://stackage.org/lts/package/hspec-expectations-json)
[![CI](https://github.com/freckle/hspec-expectations-json/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/hspec-expectations-json/actions/workflows/ci.yml)

Comparing JSON `Value`s in Haskell tests comes with some challenges:

- In API responses, additive changes are typically safe and an important way to
  evolve responses without breaking clients. Therefore, assertions against such
  responses often want to ignore any unexpected keys in `Object`s (at any
  depth), as any clients would.

- Order often doesn't matter in API responses either, so it should be possible
  to assert equality regardless of `Array` ordering (again, at any depth).

- When an assertion fails, showing the difference clearly needs to take the
  above into account (i.e. it can't show keys you've ignored, or ordering
  differences you didn't care about), and it has to display things clearly, e.g.
  as a diff.

This library handles all these things.

## Usage

**NOTE**: this is effectively a distillation of the [Haddocks][], please
view them directly for your installed version, to ensure accurate information.

[haddocks]: http://hackage.haskell.org/package/hspec-expectations-json/docs/Test-Hspec-Expectations-Json.html

Four expectations exist with the following behaviors:

| Assertion that **fails** on: | extra `Object` keys | wrong `Array` order |
| ---------------------------- | ------------------- | ------------------- |
| `shouldBeJson`               | Yes                 | Yes                 |
| `shouldBeUnorderedJson`      | Yes                 | No                  |
| `shouldMatchJson`            | No                  | No                  |
| `shouldMatchOrderedJson`     | No                  | Yes                 |

Each of these, when they fail, print a difference between the objects, where the
expected-on object has been normalized to avoid indicating any of the
differences your expectation is ignoring.

### `shouldBeJson`

Passing:

```hs
catchFailure $
  [aesonQQ| { "a": true, "b": false } |] `shouldBeJson`
  [aesonQQ| { "a": true, "b": false } |]
```

Failing:

```hs
catchFailure $
  [aesonQQ| { "a": true, "b": false } |] `shouldBeJson`
  [aesonQQ| { "a": true, "b": true  } |]
```

```diff
   {
       "a": true,
---    "b": true
+++    "b": false
   }
```

### `shouldBeUnorderedJson`

Passing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false } |] `shouldBeUnorderedJson`
  [aesonQQ| { "a": [false, true], "b": false } |]
```

Failing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldBeUnorderedJson`
  [aesonQQ| { "a": [false, true], "b": true             } |]
```

```diff
   {
       "a": [
           false,
           true
       ],
---    "b": true
+++    "b": false,
+++    "c": true
   }
```

### `shouldMatchJson`

Passing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchJson`
  [aesonQQ| { "a": [false, true], "b": false            } |]
```

Failing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchJson`
  [aesonQQ| { "a": [false, true], "b": true             } |]
```

```diff
   {
       "a": [
           false,
           true
       ],
---    "b": true
+++    "b": false
   }
```

### `shouldMatchOrderedJson`

Passing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchOrderedJson`
  [aesonQQ| { "a": [true, false], "b": false            } |]
```

Failing:

```hs
catchFailure $
  [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchOrderedJson`
  [aesonQQ| { "a": [false, true], "b": true             } |]
```

```diff
   {
       "a": [
---        false,
---        true
+++        true,
+++        false
       ],
---    "b": true
+++    "b": false
   }
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
