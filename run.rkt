#lang racket 

(system "LD_LIBRARY_PATH=\"./hquest/lib\" cabal run hquest -- data/example.qcis data/example.res")