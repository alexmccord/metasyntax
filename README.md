# Luau Grammar

Generates a `.tmLanguage` according to the language specification. Because manually writing regular expressions is nightmarish.

For now, the project tightly couples Luau with the `.tmLanguage` generation. I may look at decoupling the two in the future.

### Contributing

Get [`ghcup`](https://www.haskell.org/ghcup/), and then run any one of these:
```
cabal build
cabal test
cabal run
```
