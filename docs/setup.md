# Initial Setup

This repo is based on [Stack](https://docs.haskellstack.org/en/v1.3.0/GUIDE/).<br/>. It has been created with:

``` bash
yay -S stack-static
stack new lean-you-a-haskell kurt

```

using the template `kurt`. Other available templates are listed in [templates](https://github.com/commercialhaskell/stack-templates/wiki#simple-templates).


## Build
Run

```bash
cd learn-you-a-haskell
stack build
```

The first time this will download about 2.6G in `~/.stack` and some Mb in `.stack-work`:

```bash
❯ du -sh ~/.stack .stack-work
2.6G	/home/arialdo/.stack
4.7M	.stack-work
```

Notice that

```bash
stack clean --full
```

will clean `.stack-work`, but not `~/.stack`

### Stack, Package and Cabal
The project contains 2 important files:

* `.stack.yaml` is used by Stack. It might mention how dependencies should be resoled. That is, if at any point the system needs to build a specific package, `stack.yaml` might indicate which version to use;
* `learn-you-a-haskell.cabal` used by Cabal, a build system. Cabal defines the concept of a Package. Stack, in turn, defines the concept of a Project on top of Cabal's Packages. More on this in [Package versus Project](https://docs.haskellstack.org/en/v2.7.5/stack_yaml_vs_cabal_package_file/#package-versus-project)
 
 
#### `package.yaml` 

If the project was built using the default template `new-template` instead of `kurt`, a third file would be created, `package.yaml`. This is used by `hpack`: it adds some niceties on top of Cabal. More details can be read in the pages [stack.yaml versus package.yaml versus a Cabal file¶](https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/) and [stack.yaml vs cabal package file](https://docs.haskellstack.org/en/v2.7.5/stack_yaml_vs_cabal_package_file/) explain the difference.
