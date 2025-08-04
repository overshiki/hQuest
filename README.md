
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
cd quest-rt && racket build.rkt && cd -
```

### to test
just run 
```bash
LD_LIBRARY_PATH="./hquest/lib" cabal run hquest -- data/example.qcis data/example.res
```
where 
- `data/example.qcis` is our input file, in `QCIS` format
- `data/example.res` contains the resulting measurement bitstring, where each measurement is triggered by `QCIS` `M` gate.

## QCIS format
### file format
`QCIS` file should be surfixed with `.qcis`

### QAgent 
`QCIS` is a hardware level description language(or so-called quantum assembly language), thus it contains `QAgent` instead of `Qubit`.

`QAgent` could be `Qubit`(data qubit), `Coupler`, `Readout`. For `hQuest`, we only need `Qubit` and `Coupler`.

### Gate
`Gate` follows the format 
```
<gate> *<qagent> *<parameters>
```
example:
```
XY Q02 0.5
M Q02
M Q03 Q04
```

## QCIS extension
to support more complex behaviour of quantum simulator backend(such as hQuest), we need some extension to `QCIS` format.
```

```