module Text.Parser.RestSpect
    (   restFile
    ) where

import Control.Applicative
import Control.Monad
-- import Data.Functor.Identity (Identity)
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec hiding ( apiName )

skipSpaces :: CharParser () (Maybe ())
skipSpaces = optional $ skipMany space

stringWithUnderscores :: CharParser () String
stringWithUnderscores = many $ alphaNum <|> char '_'

listOf :: CharParser () a -> CharParser () [a]
listOf p =
        string "["
    *>  (p <* skipSpaces) `sepBy` (string "," <* skipSpaces)
    <*  string "]"

groupOf :: CharParser () a -> CharParser () [a]
groupOf p =
        string "("
    *>  (p <* skipSpaces) `sepBy` (string "," <* skipSpaces)
    <*  (skipSpaces *> string ")")

-- | parses the contents of an entire REST API file:
--
-- >>> parse restFile "(test)" "`MyAPI` servesDataTypes\n    [    data_name : data_type\n    ,    data_name : ListOf list_element\n    ,    data_name :\n   ListOf onNewLine\n    ]"
-- Right (RestSpec {apiName = ApiName "MyAPI", dataTypes = [DataType (DataTypeName "data_name") (SingleType "data_type"),DataType (DataTypeName "data_name") (ListType "list_element"),DataType (DataTypeName "data_name") (ListType "onNewLine")]})
--
-- >>> parse restFile "(test)" "`MyAPI` servesDataTypes[data_name:data_type,data_name:ListOf list_element,data_name:ListOf onNewLine]"
-- Right (RestSpec {apiName = ApiName "MyAPI", dataTypes = [DataType (DataTypeName "data_name") (SingleType "data_type"),DataType (DataTypeName "data_name") (ListType "list_element"),DataType (DataTypeName "data_name") (ListType "onNewLine")]})
--
restFile :: CharParser () RestSpec
restFile =
        RestSpec
    <$> (skipSpaces *> apiName)
    <*> (skipSpaces *> dataTypeListParser)


apiName :: CharParser () ApiName
apiName =
        (ApiName . pack)
    <$> (string "`" *> many alphaNum <* string "`")

-- | Parses a list of DataTypes.
--
-- >>> parse dataTypeListParser "(test)" "servesDataTypes [data_name : data_type, data_name : ListOf list_element, data_name :\n   ListOf onNewLine]"
-- Right [DataType (DataTypeName "data_name") (SingleType "data_type"),DataType (DataTypeName "data_name") (ListType "list_element"),DataType (DataTypeName "data_name") (ListType "onNewLine")]
--
-- >>> parse dataTypeListParser "(test)" "servesDataTypes\n    [    data_name : data_type\n    ,    data_name : ListOf list_element\n    ,    data_name :\n   ListOf onNewLine\n    ]"
-- Right [DataType (DataTypeName "data_name") (SingleType "data_type"),DataType (DataTypeName "data_name") (ListType "list_element"),DataType (DataTypeName "data_name") (ListType "onNewLine")]
--
dataTypeListParser :: CharParser () [DataType]
dataTypeListParser =
        (string "servesDataTypes" <* skipSpaces)
    *>  (skipSpaces *> listOf dataType)

-- | Parses a DataType
--
-- >>> parse dataType "(test)" "data_name : data_type"
-- Right (DataType (DataTypeName "data_name") (SingleType "data_type"))
--
-- >>> parse dataType "(test)" "data_name : ListOf list_element"
-- Right (DataType (DataTypeName "data_name") (ListType "list_element"))
--
-- >>> parse dataType "(test)" "data_name :\n   ListOf onNewLine"
-- Right (DataType (DataTypeName "data_name") (ListType "onNewLine"))
--
dataType :: CharParser () DataType
dataType = DataType
    <$> (dataTypeName
        <* (skipSpaces *> string ":"))
    <*> (skipSpaces *> dataTypeType)

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
--
dataTypeType :: CharParser () DataTypeType
dataTypeType =
        try ((ListType . pack)
    <$> ((string "ListOf" *> skipSpaces) *> stringWithUnderscores))
    <|> (SingleType . pack) <$> (skipSpaces *> stringWithUnderscores)

eol :: CharParser () String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
