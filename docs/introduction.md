# Introduction

[Introduction](http://learnyouahaskell.com/introduction#about-this-tutorial)

## Run GHCI

```bash
stack ghci
```

In `learn-you-a-haskell/src/Lib.hs` we have

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

From GHCI, the module `Lib` can be loaded with:


```bash
:load Lib
```

or `:l Lib`. `someFunc` can be evaluated with:

```bash
someFun
```

or via its fully qualified name:

```bash
Lib.someFun
```

## Exit
Press `C-d` to exit.




