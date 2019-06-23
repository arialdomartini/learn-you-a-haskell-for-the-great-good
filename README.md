Learn You A Haskell For The Great Good
==============================================

Exercises and notes while reading [Learn You A Haskell For The Great Good](http://learnyouahaskell.com/) by Miran Lipovača.

# Usage
Either install Haskell or use the command `run.sh`, based on Docker.

## Run an interactive console
Just run:

```bash
./console.sh
```

The relative Docker container has visibility to the current directory, so for example it's possible to execute:

```haskell
*Main> :l chapter-1/lists.hs
[1 of 1] Compiling Main             ( chapter-1/lists.hs, interpreted )
Ok, one module loaded.
*Main> printList
[4,8,15,16,23,42]
```

To cleanup the filesystem from the `.hi` and the `.o` files, run:

```bash
./clean.sh
```
## Compile with Docker
Run:

```bash
./run.sh <haskell-source-file.hs>
```

which compiles the source code with

```bash
ghc -o /tmp/runit <haskell-source-file.hs>
```

and finally run the compiled `/tmp/runit` file. `/tmp/runit` is overwritten at each compilation.
