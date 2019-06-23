docker run --rm -v /tmp:/tmp -v $(pwd):/source -w /source haskell ghc -o /tmp/runit $1 && /tmp/runit
