{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Expr 
import Parse 
import ParseUtils

import Foreign.C.Types
-- import Foreign.StablePtr
import Foreign.Ptr
import Foreign
import System.Environment (getArgs)

-- foreign import ccall "test"
--   test :: IO ()

foreign import ccall "prog"
  -- numQubits, prog_length, ps, ts
  prog :: CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()

foreign import ccall "dmProg"
  -- numQubits, prog_length, ps, ts
  dmProg :: CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()

int2cint :: Int -> CInt
int2cint = CInt . fromIntegral

-- testProg :: IO ()
-- testProg = do
--   putStrLn "Hello, Haskell!"
--   -- test
--   psPtr <- newArray [0, 1, 0, 0, 2, 0]
--   tsPtr <- newArray [0.0 :: CDouble]
--   let 
--     nqubit = int2cint 3
--     progLength = int2cint 2
--   prog nqubit progLength psPtr tsPtr

runCircuit :: Circuit -> IO [Int]
runCircuit c@(Circuit gs) = do 
  let 
    ps = encoding c
    ts = collectThetas c
    nMeasures = numMeasures c
  psPtr <- newArray $ map int2cint ps 
  tsPtr <- newArray $ map CDouble ts
  nsPtr <- newArray $ map int2cint (take nMeasures $ repeat (-1))
  let 
    nqubit = int2cint $ numQubits c
    progLength = int2cint (length gs)
  if (isDensityMatrix c) 
    then dmProg nqubit progLength psPtr tsPtr nsPtr
    else prog nqubit progLength psPtr tsPtr nsPtr
  ms <- peekArray nMeasures nsPtr
  -- print ms
  return $ map (\(CInt i) -> fromIntegral i) ms

testProg :: IO ()
testProg = do
  let file = "data/example.qcis"
  s <- readFile file
  let circ = run parseCircuit ("!!!Start " ++ s) 
  print circ
  runCircuit circ 
  return ()

main :: IO ()
main = do
  -- let file = "data/example.qcis"
  file:ofile:_ <- getArgs
  s <- readFile file
  let circ = run parseCircuit ("!!!Start " ++ s) 
  print circ
  ms <- runCircuit circ
  let 
    res = foldl ( ++ ) "" $ map show ms 
  writeFile ofile res 