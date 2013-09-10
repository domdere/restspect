module Text.Parser.Restspect where

import Control.Applicative
import Control.Monad
-- import Data.Functor.Identity (Identity)
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec hiding ( apiName )

-- | parses the contents of an entire REST API file:
--
-- >> parse restFile "(test)" "Name: hello\n"
-- Right [RestSpec {apiName = ApiName "hello"}]
--
-- >> parse restFile "(test)" "Name hello\n"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "Name: "
--
-- >> parse restFile "(test)" "Random Pattern"
-- Right []
--
restFile :: CharParser () RestSpec
restFile = RestSpec <$> apiName <*> dataTypeListParser

skipSpaces :: CharParser () ()
skipSpaces = skipMany1 space

listOf :: CharParser () a -> CharParser () [a]
listOf p = string "[" *> p `sepBy` string "," <* string "]"

groupOf :: CharParser () a -> CharParser () [a]
groupOf p = string "(" *> p `sepBy` string "," <* string ")"

apiName :: CharParser () ApiName
apiName = (ApiName . pack) <$> (string "`" *> many alphaNum <* string "`")

dataTypeListParser :: CharParser () [DataType]
dataTypeListParser =
    string "servesDataTypes" *> listOf dataType

dataType :: CharParser () DataType
dataType = DataType <$> (dataTypeName <* string ":") <*> dataTypeType

dataTypeName :: CharParser () DataTypeName
dataTypeName = (DataTypeName . pack) <$> many alphaNum

-- | Parses the Type of a DataType
--
-- >>> parse dataTypeType "(test)" "ListOf Apple"
-- Right (ListType "Apple")
--
-- >>> parse dataTypeType "(test)" "Orange"
-- Right (SingleType "Orange")
dataTypeType :: CharParser () DataTypeType
dataTypeType =
        try ((ListType . pack) <$> ((string "ListOf" *> skipSpaces) *> many alphaNum))
    <|> (SingleType . pack) <$> many alphaNum

eol :: CharParser () String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
