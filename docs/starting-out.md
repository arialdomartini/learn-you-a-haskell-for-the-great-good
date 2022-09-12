# Starting Out

[Starting Out](http://learnyouahaskell.com/starting-out)

## Making the GHCI prompt shorter

From GHCI type:

```bash
:set prompt  "λ: "
```

To make this permanent, create the file `~/.ghci` with the same content.

## Run tests
Tests are hosted in the `test` directory. Each module must be referenced in `learn-you-a-haskell.cabal`.

Run the tests with:

```bash
stack test
```
