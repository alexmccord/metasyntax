# Luau Grammar

Generates a `.tmLanguage` according to the language specification. Because manually writing regular expressions is nightmarish.

For now, the project tightly couples Luau with the `.tmLanguage` generation. I may look at decoupling the two in the future.

### Contributing

You will need `GHC >= 9.4.1 && < 9.6.1` which you can get from [`ghcup`](https://www.haskell.org/ghcup/). Then run any one of these:
```
cabal build
cabal test
cabal run
```

### CI

For CI, we use [haskell-ci](https://github.com/haskell-CI/haskell-ci) to generate the workflow. If you have a different GHC version and all test passes, please edit the `tested-with` in cabal file with such version, and then run `haskell-ci regenerate`.
