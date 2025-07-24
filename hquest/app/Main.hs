{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Expr 
import Parse 
import ParseUtils

import Foreign.C.Types
-- import Foreign.StablePtr
import Foreign.Ptr
import Foreign

foreign import ccall "test"
  test :: IO ()

foreign import ccall "prog"
  -- numQubits, prog_length, ps, ts
  prog :: CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO ()

int2cint :: Int -> CInt
int2cint = CInt . fromIntegral

testProg :: IO ()
testProg = do
  putStrLn "Hello, Haskell!"
  -- test
  psPtr <- newArray [0, 1, 0, 0, 2, 0]
  tsPtr <- newArray [0.0 :: CDouble]
  let 
    nqubit = int2cint 3
    progLength = int2cint 2
  prog nqubit progLength psPtr tsPtr

runCircuit :: Circuit -> IO ()
runCircuit c@(Circuit gs) = do 
  let 
    ps = encoding c
    ts = collectThetas c
  psPtr <- newArray $ map int2cint ps 
  tsPtr <- newArray $ map CDouble ts
    -- [0.0 :: CDouble]
  let 
    nqubit = int2cint 3
    progLength = int2cint (length gs)
  prog nqubit progLength psPtr tsPtr


main :: IO ()
main = do
  let file = "data/example.qcis"
  s <- readFile file
  let circ = run parseCircuit ("!!!Start " ++ s) 
  print circ
  runCircuit circ 

