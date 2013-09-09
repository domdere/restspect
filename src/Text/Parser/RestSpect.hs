module Text.Parser.Restspect where

import Control.Applicative
import Control.Monad
-- import Data.Functor.Identity (Identity)
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec

-- | parses the contents of an entire REST API file:
--
-- >>> parse restFile "(test)" "Name: hello\n"
-- Right [RestSpec {apiName = ApiName "hello"}]
--
-- >>> parse restFile "(test)" "Name hello\n"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "Name: "
--
-- >>> parse restFile "(test)" "Random Pattern"
-- Right []
--
restFile :: CharParser () [RestSpec]
restFile = restAPI `endBy` eol

restAPI :: CharParser () RestSpec
restAPI = RestSpec <$> (string "Name: " *> ((ApiName . pack) `fmap` many alphaNum))

eol :: CharParser () String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
