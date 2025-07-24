{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Qcis where 

data SingleQcis = 
  X | Y | Z
  | X2P | X2M | Y2P | Y2M 
  | S | SD | T | TD 
  | RZ 
  | XY | XY2P | XY2M 

data DoubleQcis = CZ 

data Ph = Ph Float
  | Pint Int
  | Sin Ph 
  | Cos Ph
  | Div Ph Ph 
  | Plus Ph Ph
  | Sqrt Ph 
  | Pzero
  | Pone

negOne :: Ph 
negOne = Pint (-1)

data Complex = Complex Ph Ph 
  | Exp Complex 
  | Coeff Ph Complex
  | Zero 
  | One 

liftReal :: Ph -> Complex
liftReal ph = Complex ph Pzero

liftImag :: Ph -> Complex
liftImag ph = Complex Pzero ph

img :: Complex
img = Complex Pzero Pone

data Matrix = Matrix [[Complex]] 
  | CoeffMatrix Ph Matrix

class HasRep a where 
  getRep :: a -> Matrix 

instance HasRep SingleQcis where 
  getRep qcis = case qcis of 
    X -> Matrix
      [ [Zero, One]
      , [One, Zero]
      ]
    Y -> Matrix 
      [ [Zero, Coeff negOne img]
      , [img, Zero]
      ]
    Z -> Matrix 
      [ [One, Zero]
      , [Zero, liftReal negOne]
      ]
    -- X2P -> 
    --   CoeffMatrix
    --     $ (Div One (Sqrt (Pint 2)))
    --     $ Matrix 
    --       [ [One, liftImag negOne] 
    --       , [liftImag negOne, One]
    --       ]
    _ -> error "value error"
