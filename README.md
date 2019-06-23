Learn You A Haskell For The Great Good
==============================================

Exercises and notes while reading [Learn You A Haskell For The Great Good](http://learnyouahaskell.com/) by Miran Lipovača.

# Usage
Either install Haskell or use the command `run.sh`, based on Docker.

## Use with Docker
Run:

```bash
./run.sh <haskell-source-file.hs>
```

which compiles the source code with

```bash
ghc -o /tmp/runit <haskell-source-file.hs>
```

and finally run the compiled `/tmp/runit` file. `/tmp/runit` is overwritten at each compilation.
