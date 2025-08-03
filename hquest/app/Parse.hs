{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse where
import Expr
import ParseUtils
import Control.Monad
import Data.List
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe
import qualified Data.HashMap.Strict as HS
import Control.Monad.State.Lazy

parseExhaust :: Parser a -> Parser [a]
parseExhaust parseElm = safeManyTill parseElm (notFollowedBy parseElm)

parseQ :: Parser Q
parseQ = do
  lstring "Q"
  Q <$> parseInt

parseShow :: (Show a) => a -> Parser a
parseShow x = lstring (show x) >> return x

parseEnum :: (Show a) => [a] -> Parser a
parseEnum (x:xs) = do
  let
    parseTail = msum $ try . parseShow <$> xs
  parseTail <|> parseShow x
parseEnum [] = error "value error"

parseGateTy :: Parser GateTy
parseGateTy = parseEnum gateTyList

parseGateHd :: Parser GateHd
parseGateHd =
  try (Gty <$> parseGateTy)
  <|> (lstring "defkraus" >> return DefKraus)

data Ind = PI Int | PF Float
  deriving (Eq, Show)

parseInd :: Parser Ind
parseInd = try (PF <$> parseFloat)
  <|> (PI <$> parseInt)

forceParseFloat :: Parser Float
forceParseFloat = do
  d <- parseInd
  case d of
    PF f -> return f
    PI i -> return $ fromIntegral i

parseComplex :: Parser Complex
parseComplex = do
  r <- forceParseFloat
  lstring "+"
  i <- forceParseFloat
  lstring "j"
  return $ Complex (r, i)

parseComplexMatrix :: Int -> Parser [[Complex]]
parseComplexMatrix n = parseMatrix n parseComplex

parseKrausName :: Parser String 
parseKrausName = do 
  lstring "K"
  v <- parseVar 
  return $ "K" ++ v

parseKrausOp :: Parser ()
parseKrausOp = do
  v <- parseKrausName
  dn <- parseInt
  lstring "x"
  dn' <- parseInt
  if dn /= dn'
    then error "value error"
    else do
      ds <- parseComplexMatrix dn
      let kops = KrausOp dn ds
      env <- get
      let nenv = HS.insert v kops env
      put nenv
      return ()

parseGate :: Parser (Maybe Gate)
parseGate = do
  ghd <- parseGateHd
  case ghd of
    Gty Kraus -> do
      qs <- parseExhaust parseQ
      ks <- parseExhaust parseKrausName
      env <- get
      let check = all (`elem` HS.keys env) ks
      if check
        then return $ Just $ Gate Kraus qs [] ks
        else error "kraus name not fount in env"
    Gty gty -> do
      qs <- parseExhaust parseQ
      ts <- parseExhaust parseDouble
      return $ Just $ Gate gty qs ts []
    DefKraus ->
      parseKrausOp >> return Nothing

parseStart :: Parser ()
parseStart = void (lstring "!!!Start")

parseCircuit :: Parser Circuit
parseCircuit = parseStart >>
  (Circuit . catMaybes <$> safeManyTill parseGate eof)