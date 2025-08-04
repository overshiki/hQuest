{-# LANGUAGE TypeFamilies #-}
module Expr where
import qualified Data.List as L
import qualified Data.HashMap.Strict as HS
import Data.Maybe
import Control.Monad.State.Lazy
import GHC.Float (float2Double)
-- import System.IO.Unsafe

type Env = HS.HashMap String KrausOp

type IndexedEnv = HS.HashMap String Int

getIndexedEnv :: Env -> IndexedEnv
getIndexedEnv d = HS.fromList (zip ks [0 .. ]) 
  where 
    ks = L.sort $ HS.keys d

-- (flattenArray, dimArray)
flattenEnv :: (Env, IndexedEnv) -> ([Double], [Int])
flattenEnv (env, indexenv) = (vs, dims)
  where 
    revIndexedEnv = HS.fromList $ map (\(s, i) -> (i, s)) $ HS.toList indexenv
    nKraus = L.length $ HS.keys revIndexedEnv
    vds = map 
      (\i -> 
        let s = fromJust $ HS.lookup i revIndexedEnv in 
        let (KrausOp dim ds) = fromJust $ HS.lookup s env in
        let vsMini = concatMap (\(Complex (x, y)) -> [x, y]) $ concat ds in 
        (vsMini, dim)
      ) 
      [0 .. nKraus - 1]
    (vs_, dims) = unzip vds    
    vs = concat vs_

-- Env, IndexedEnv, jump indices for 1q kraus, jump indices for 2q kraus
type EnvState = State (Env, IndexedEnv, [Int], [Int])

class HasEncode a where
  type En a
  encoding :: a -> EnvState (En a)

data GateTy =
  CZ
  | SD | S
  | TD | T
  | XY2P | XY2M
  | X2P | X2M
  | Y2P | Y2M
  | XY
  | X | Y | Z | M | Kraus
  deriving (Show, Eq, Ord, Bounded, Enum)

-- property of dataclass Enum from GHC.Enum
gateTyList :: [GateTy]
gateTyList = [CZ ..]

instance HasEncode GateTy where
  type En GateTy = Int
  encoding gt = return $ case gt of
    X   -> 0
    Y   -> 1
    X2P -> 2
    X2M -> 3
    Y2P -> 4
    Y2M -> 5
    Z   -> 6
    S   -> 7
    SD  -> 8
    T   -> 9
    TD  -> 10
    XY  -> 11
    XY2P -> 12
    XY2M -> 13
    CZ   -> 14
    M    -> 15
    Kraus -> 16

data GateHd = Gty GateTy
  | DefKraus
  deriving (Eq, Show)

newtype Q = Q Int
  deriving (Eq, Show)

q2int :: Q -> Int 
q2int (Q i) = i

instance HasEncode Q where
  type En Q = Int
  encoding (Q i) = return i

newtype Complex = Complex (Double, Double)
  deriving (Eq, Show)

-- KrausOp ndim data
data KrausOp = KrausOp Int [[Complex]] 
  deriving (Eq, Show)

data Gate = Gate GateTy [Q] [Double] [String]
  deriving (Show)

instance HasEncode Gate where
  type En Gate = [Int]
  encoding (Gate Kraus qs _ ps) = case qs of 
    -- we need two-level jump table for this encoding
    -- mainly, the final index represents the position in the indices of EnvState(these indices represents the indices for Env)
    -- currently only support one-qubit operation
    -- TODO: support multi-qubits operation
    [x] -> do
      (env, indexenv, indices1q, indices2q) <- get
      let 
        channelIndices = map (\p -> fromJust $ HS.lookup p indexenv) ps
        offsetL = L.length indices1q
        nindices1q = indices1q ++ channelIndices
        offsetR = L.length nindices1q

      put (env, indexenv, nindices1q, indices2q)

      engt <- encoding Kraus 
      enx  <- encoding x 
      return [engt, enx, -1, offsetL, offsetR]

    [x, y] -> do
      (env, indexenv, indices1q, indices2q) <- get
      let 
        channelIndices = map (\p -> fromJust $ HS.lookup p indexenv) ps
        offsetL = L.length indices2q
        nindices2q = indices2q ++ channelIndices
        offsetR = L.length nindices2q

      put (env, indexenv, indices1q, nindices2q)

      engt <- encoding Kraus 
      enx  <- encoding x
      eny  <- encoding y
      return [engt, enx, eny, offsetL, offsetR]

    _ -> error "currently, we do not support multi-qubits operation for quantum channel. Maybe in the future"
  encoding (Gate gt qs _ _) = case qs of
    [x] -> do 
      engt <- encoding gt 
      enx  <- encoding x 
      return [engt, enx, -1, -1, -1] 
    [x, y] -> do 
      engt <- encoding gt 
      enx  <- encoding x 
      eny  <- encoding y 
      return [engt, enx, eny, -1, -1]
    _ -> error "value error"
    

newtype Circuit = Circuit [Gate]
  deriving (Show)

instance HasEncode Circuit where 
  type En Circuit = [Int]
  encoding (Circuit gs) = concat <$> mapM encoding gs

collectThetas :: Circuit -> [Double]
collectThetas (Circuit ((Gate _ _ ds _):gs)) = ds ++ collectThetas (Circuit gs)
collectThetas (Circuit []) = []

collectQubits :: Circuit -> [Q]
collectQubits (Circuit ((Gate _ qs _ _):gs)) = L.nub $ qs ++ collectQubits (Circuit gs)
collectQubits (Circuit []) = []

numQubits :: Circuit -> Int 
numQubits c = L.maximum qs + 1
  where 
    qs = map q2int $ collectQubits c


numMeasures_ :: Int -> Circuit -> Int 
numMeasures_ count (Circuit ((Gate gt _ _ _):gs)) = case gt of 
  M -> numMeasures_ (count + 1) (Circuit gs)
  _ -> numMeasures_ count (Circuit gs)
numMeasures_ count (Circuit []) = count

numMeasures :: Circuit -> Int 
numMeasures = numMeasures_ 0

isDensityMatrix :: Circuit -> Bool 
isDensityMatrix (Circuit ((Gate Kraus _ _ _):_)) = True 
isDensityMatrix (Circuit (_:gs)) = isDensityMatrix (Circuit gs)
isDensityMatrix (Circuit []) = False