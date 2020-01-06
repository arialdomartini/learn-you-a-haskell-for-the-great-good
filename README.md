# Code and exercises reading [Learn you a Haskell](http://learnyouahaskell.com)

## Usage

* `./build.sh` creates the Docker image `arialdomartini/haskell-stack`, based on `fpco/stack-build` and including `docker/.ghci`;
* `./run.sh` runs a Haskell file in a Docker container;
* `./console.sh` runs a Bash with support to Haskell


## Example

```bash
./run.sh src/Chapter1/hello-world.hs 
```

To run tests, enter the console with

```bash
./console.sh
```
and then run:

```bash
stack test
```

### Troubleshooting
You might want to delete `.stack-cache` and `.stack-work`. 
