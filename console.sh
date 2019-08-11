mkdir -p .stack-work &&
./build.sh &&
docker run -ti --rm \
       -v $(pwd)/.stack-cache:/root/.stack/ \
       -v $(pwd):/source \
       -w /source \
       arialdomartini/haskell-stack bash
