
## Introduction
`hQuest` is a simulation tool for quantum computation development in `Haskell`, using [QuEST](https://github.com/QuEST-Kit/QuEST/tree/main) as its backend

`hQuest` is a very thin layer of wrapper using runtime idea. Specifically, we wrote a simple `cpp` script to build a `runtime` using `QuEST`, the `runtime` recieves sequences of quantum instructions(such as QCIS) as input, perform simulation using `QuEST` and output the results. We then `FFI` the interface of the `runtime`(as a shared library) into `Haskell`, with a simple `parser` to parse the txt file(for example, ./data/example.qcis) and then pass the resulting bytecode into `runtime`.

### dependency
we need `haskell`, the `dev` environment can be obtained easily following the guidance of [ghcup](https://www.haskell.org/ghcup/)

we also need g++ (version 11) installed 

the build script is in `racket`, could be easily installed in ubuntu though `apt-get`:
```bash
sudo apt-get install racket
```

### build
to build, firstly, update submodule:
```bash
git submodule update --init --recursive
```

then
```
cd quest-rt && racket build.rkt
```

### to test
just run 
```bash
LD_LIBRARY_PATH="./hquest/lib" cabal run hquest
```