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

parseGate :: Parser Gate
parseGate = do
  gty <- parseGateTy
  -- qs <- safeManyTill parseQ preparseNext
  qs <- parseExhaust parseQ
  ts <- parseExhaust parseDouble
  return $ Gate gty qs ts

parseStart :: Parser ()
parseStart = void (lstring "!!!Start") 

parseCircuit :: Parser Circuit 
parseCircuit = parseStart >>
  (Circuit <$> safeManyTill parseGate eof)
  