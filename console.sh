mkdir -p .stack-work
docker run -ti --rm -v $(pwd)/.stack-cache:/root/.stack/ -v $(pwd):/source -w /source fpco/stack-build bash
