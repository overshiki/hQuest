{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}
module ParseUtils where
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import System.IO
import Control.Applicative
import GHC.Stack (HasCallStack)
import Control.Monad.State.Lazy

import qualified Data.HashMap.Strict as HS
import qualified Data.Set as Set
import Data.Maybe
import GHC.Float (float2Double)

type Parser = Parsec Void String
-- type Parser = ParsecT Void String (State Env)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "\\")
  (L.skipBlockComment "(*" "*)")

-- cosume all the white spaces following a Parser
-- white spaces include: " " "\n" "\t"
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lstring :: String -> Parser String
lstring = lexeme . string

safeManyTill :: MonadParsec e s f => f a -> f b -> f [a]
safeManyTill p end = go
  where
    go = try ([] <$ end) <|> liftA2 (:) p go

manyBetween :: Parser a -> Parser a -> Parser String
manyBetween s e = s *> safeManyTill L.charLiteral e

run :: HasCallStack => Parser a -> String -> a
run p s = case m of
  Left bundle -> error (errorBundlePretty bundle)
  Right r -> r
  where
   m = runParser p "" s

parseInt :: Parser Int
parseInt = lexeme $ L.signed sc L.decimal

parseFloat :: Parser Float 
parseFloat = lexeme $ L.signed sc L.float

parseDouble :: Parser Double 
parseDouble = float2Double <$> parseFloat

parseVar :: Parser String
parseVar = do
  lexeme $ safeManyTill L.charLiteral (lookAhead (try space1 <|> eof))

excludePredict :: Parser a -> Parser ()
excludePredict p = lookAhead $ notFollowedBy p

