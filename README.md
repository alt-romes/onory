    Onory is a high-level language embedded in Haskell for
    specifying distributed algorithms/systems using common
    notation seen accross papers in the area. The built-in
    interpreter enables a distributed system specified in Onory
    to be compiled to an executable node.
    
    Being able to execute the high-level specification of a
    distributed protocol allows faster iteration and
    observability during the development of existing, or novel,
    protocols. Moreover, the executable derived from the
    specification is automatically able to parse protocol
    configuration options from the command line interface, and
    trace all events being triggered and handled by the running
    protocol, making experimentation easy.

# Installation (WIP)

As a pre-requisite to using `onory`, you need to have a working installation of
the Glasgow Haskell Compiler and the Haskell build tool, Cabal.  The lowest
friction way to get GHC/Cabal is using [GHCup](https://www.haskell.org/ghcup/).

In certain distributions there may be up-to-date packaged versions of the
compiler (typically `ghc`) and of build tool (typically listed under
`cabal-install`). Note that only GHC == 9.4.8 and GHC == 9.8.2 are actively
tested.

The `onoryc` tool compiles a standalone Onory specification into an executable
protocol. Under the hood, it's invoking Cabal to fetch the onory dependency and
GHC to compile the specification (recall that an Onory specification is embedded
in Haskell).

You can install the `onoryc` tool by downloading it from this repository's master branch archive into `$PATH` and making it executable:

```
curl -LO https://raw.githubusercontent.com/alt-romes/onory/main/onoryc
chmod +x onoryc
```

Copy one of the examples into a standalone `.hs` file and try compiling it with
```
./onoryc <file.hs>
```
