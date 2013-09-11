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

skipSpaces :: CharParser () (Maybe ())
skipSpaces = optional $ skipMany1 space

stringWithUnderscores :: CharParser () String
stringWithUnderscores = many $ alphaNum <|> char '_'

listOf :: CharParser () a -> CharParser () [a]
listOf p = string "[" *> p `sepBy` string "," <* string "]"

groupOf :: CharParser () a -> CharParser () [a]
groupOf p = string "(" *> p `sepBy` string "," <* string ")"

apiName :: CharParser () ApiName
apiName = (ApiName . pack)
    <$> (string "`" *> many alphaNum <* string "`")

dataTypeListParser :: CharParser () [DataType]
dataTypeListParser =
    string "servesDataTypes" *> listOf dataType

-- | Parses a DataType
--
-- >>> parse dataType "(test)" "data_name : data_type"
-- Right (DataType (DataTypeName "data_name") (SingleType "data_type"))
--
dataType :: CharParser () DataType
dataType = DataType
    <$> (dataTypeName
        <* (skipSpaces *> string ":"))
    <*> dataTypeType

-- | Parses the Name of a DataType
--
-- >>> parse dataTypeName "(test)" "SomeName"
-- Right (DataTypeName "SomeName")
--
-- >>> parse dataTypeName "(test)" "  \nSomeName   "
-- Right (DataTypeName "SomeName")
--
dataTypeName :: CharParser () DataTypeName
dataTypeName = (DataTypeName . pack) <$> (skipSpaces *> stringWithUnderscores)

-- | Parses the Type of a DataType
--
-- >>> parse dataTypeType "(test)" "ListOf Apple"
-- Right (ListType "Apple")
--
-- >>> parse dataTypeType "(test)" "Orange"
-- Right (SingleType "Orange")
--
-- >>> parse dataTypeType "(test)" "[]\n"
-- Right (SingleType "")
dataTypeType :: CharParser () DataTypeType
dataTypeType =
        try ((ListType . pack) <$> ((string "ListOf" *> skipSpaces) *> stringWithUnderscores))
    <|> (SingleType . pack) <$> (skipSpaces *> stringWithUnderscores)

eol :: CharParser () String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
