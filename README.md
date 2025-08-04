
## Introduction
`hQuest` is a simulation tool for quantum computation development in `Haskell`, using [QuEST](https://github.com/QuEST-Kit/QuEST/tree/main) as its backend

`hQuest` is a very thin layer of wrapper using runtime idea. Specifically, we wrote a simple `cpp` script to build a `runtime` using `QuEST`, the `runtime` receives sequences of quantum instructions(such as QCIS) as input, perform simulation using `QuEST` and output the results. We then `FFI` the interface of the `runtime`(as a shared library) into `Haskell`, with a simple `parser` to parse the txt file(for example, ./data/example.qcis) and then pass the resulting bytecode into `runtime`.

### Dependency
we need `haskell`, the `dev` environment can be obtained easily following the guidance of [ghcup](https://www.haskell.org/ghcup/)

we also need g++ (version 11) installed 

the build script is in [racket](https://racket-lang.org/)(you do not need to learn it, you can just use the build script as a command-line tool), which could be easily installed in ubuntu though `apt-get`:
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
`QCIS` file should be suffixed with `.qcis`

### Qagent 
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
to support more complex behavior of quantum simulator backend(such as hQuest), we need some extension to `QCIS` format.

- `defkraus` is used to declare a kraus operator, and bind it to a name
```
defkraus K1 2x2 [[0.7071067811865476+0j, 0+0j], [0+0j, 0.7071067811865476+0j]]
```

- `Kraus` is used to declare a quantum channel on the target qubit, using the kraus operator declared previously using `defkraus`
```
defkraus K1 2x2 [[0.7071067811865476+0j, 0+0j], [0+0j, 0.7071067811865476+0j]]
defkraus K2 2x2 [[0+0j, 0.7071067811865476+0j], [0.7071067811865476+0j, 0+0j]] 
Kraus Q01 K1 K2
```

## Comment block 
to comment out code block in `.qcis` file, use `\\` for single line and `(**)` for multiple lines.

example:
```
X Q01
Y Q02
XY Q02 0.5
XY Q03 0.5
\\ defkraus K1 2x2 [[0.7071067811865476+0j, 0+0j], [0+0j, 0.7071067811865476+0j]]
\\ defkraus K2 2x2 [[0+0j, 0.7071067811865476+0j], [0.7071067811865476+0j, 0+0j]] 
\\ Kraus Q01 K1 K2
M Q02
M Q01
M Q03
```
will comment out `defkraus` and `Kraus` lines.

```
X Q01
Y Q02
XY Q02 0.5
XY Q03 0.5
(* 
defkraus K1 2x2 [[0.7071067811865476+0j, 0+0j], [0+0j, 0.7071067811865476+0j]]
defkraus K2 2x2 [[0+0j, 0.7071067811865476+0j], [0.7071067811865476+0j, 0+0j]] 
Kraus Q01 K1 K2
*)
M Q02
M Q01
M Q03
```
works the same way.