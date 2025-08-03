{-# LANGUAGE TypeFamilies #-}
module Expr where
import qualified Data.List as L

class HasEncode a where
  type En a
  encoding :: a -> En a

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
  encoding gt = case gt of
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
  encoding (Q i) = i

newtype Complex = Complex (Float, Float)
  deriving (Eq, Show)

-- KrausOp ndim data
data KrausOp = KrausOp Int [[Complex]] 
  deriving (Eq, Show)

data Gate = Gate GateTy [Q] [Double]
  deriving (Show)

instance HasEncode Gate where
  type En Gate = [Int]
  encoding (Gate gt qs _) = case qs of
    [x]    -> [encoding gt, encoding x, 0]
    [x, y] -> [encoding gt, encoding x, encoding y]
    _ -> error "value error"

newtype Circuit = Circuit [Gate]
  deriving (Show)

instance HasEncode Circuit where 
  type En Circuit = [Int]
  encoding (Circuit gs) = concatMap encoding gs

collectThetas :: Circuit -> [Double]
collectThetas (Circuit ((Gate _ _ ds):gs)) = ds ++ collectThetas (Circuit gs)
collectThetas (Circuit []) = []

collectQubits :: Circuit -> [Q]
collectQubits (Circuit ((Gate _ qs _):gs)) = L.nub $ qs ++ collectQubits (Circuit gs)
collectQubits (Circuit []) = []

numQubits :: Circuit -> Int 
numQubits c = L.maximum qs + 1
  where 
    qs = map q2int $ collectQubits c


numMeasures_ :: Int -> Circuit -> Int 
numMeasures_ count (Circuit ((Gate gt _ _):gs)) = case gt of 
  M -> numMeasures_ (count + 1) (Circuit gs)
  _ -> numMeasures_ count (Circuit gs)
numMeasures_ count (Circuit []) = count

numMeasures :: Circuit -> Int 
numMeasures = numMeasures_ 0

isDensityMatrix :: Circuit -> Bool 
isDensityMatrix (Circuit ((Gate Kraus _ _):_)) = True 
isDensityMatrix (Circuit (_:gs)) = isDensityMatrix (Circuit gs)
isDensityMatrix (Circuit []) = False