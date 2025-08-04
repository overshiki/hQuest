{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where
import Expr
import Parse
import ParseUtils
import qualified Data.HashMap.Strict as HS
import Control.Monad.State.Lazy
import Foreign.C.Types
-- import Foreign.StablePtr
import Foreign.Ptr
import Foreign
import System.Environment (getArgs)

-- foreign import ccall "test"
--   test :: IO ()

foreign import ccall "prog"
  prog :: 
    CInt              -- int numQubits
    -> CInt           -- int prog_length
    -> Ptr CInt       -- int* ps
    -> Ptr CDouble    -- double* ts
    -> Ptr CInt       -- int* measures
    -> IO ()

foreign import ccall "dmProg"
  dmProg :: 
    CInt              -- int numQubits
    -> CInt           -- int prog_length 
    -> Ptr CInt       -- int* ps
    -> Ptr CDouble    -- double* ts
    -> Ptr CInt       -- int* measures
    -> Ptr CInt       -- int* dims
    -> Ptr CInt       -- int* channelIndicesQ1
    -> Ptr CInt       -- int* channelIndicesQ2
    -> Ptr CDouble    -- double* krausVec
    -> IO ()

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

runCircuit :: Circuit -> Env -> IO [Int]
runCircuit c@(Circuit gs) env = do
  let
    indexedEnv = getIndexedEnv env
    (ks, dims) = flattenEnv (env, indexedEnv)
    (ps, (_, _, channelIndices1q, channelIndices2q)) = runState (encoding c) (env, indexedEnv, [], [])
    ts = collectThetas c
    nMeasures = numMeasures c
  psPtr <- newArray $ map int2cint ps
  tsPtr <- newArray $ map CDouble ts
  nsPtr <- newArray $ replicate nMeasures (int2cint (-1))
  dimsPtr <- newArray $ map int2cint dims
  ch1qPtr <- newArray $ map int2cint channelIndices1q
  ch2qPtr <- newArray $ map int2cint channelIndices2q
  ksPtr <- newArray $ map CDouble ks
  let
    nqubit = int2cint $ numQubits c
    progLength = int2cint (length gs)
  if isDensityMatrix c
    then dmProg nqubit progLength psPtr tsPtr nsPtr dimsPtr ch1qPtr ch2qPtr ksPtr
    else prog nqubit progLength psPtr tsPtr nsPtr
  ms <- peekArray nMeasures nsPtr
  -- print ms
  return $ map (\(CInt i) -> fromIntegral i) ms

testProg :: IO ()
testProg = do
  let file = "data/example.qcis"
  s <- readFile file
  let (circ, env) = run parseCircuit ("!!!Start " ++ s)
  print circ
  runCircuit circ env
  return ()

main :: IO ()
main = do
  -- let file = "data/example.qcis"
  file:ofile:_ <- getArgs
  s <- readFile file
  let (circ, env) = run parseCircuit ("!!!Start " ++ s)
  print circ
  print $ HS.toList env
  ms <- runCircuit circ env
  let
    res = concatMap show ms
  writeFile ofile res