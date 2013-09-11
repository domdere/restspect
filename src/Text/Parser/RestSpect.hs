module Text.Parser.Restspect where

import Control.Applicative
import Control.Monad
-- import Data.Functor.Identity (Identity)
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec hiding ( apiName )

skipSpaces :: CharParser () (Maybe ())
skipSpaces = optional $ skipMany1 space

stringWithUnderscores :: CharParser () String
stringWithUnderscores = (:) <$> charParser <*> many charParser
    where
        charParser = alphaNum <|> char '_'

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
-- >> parse restFile "(test)" "Name: hello\n"
-- Right [RestSpec {apiName = ApiName "hello"}]
--
-- >> parse restFile "(test)" "Name hello\n"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "Name: "
--
-- >>> parse restFile "(test)" "`MyAPI` servesDataTypes\n    [    data_name : data_type\n    ,    data_name : ListOf list_element\n    ,    data_name :\n   ListOf onNewLine\n    ]"
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
-- >>> parse dataTypeType "(test)" "ListOf \n  \n Apple"
-- Right (ListType "Apple")
--
-- >>> parse dataTypeType "(test)" "Orange"
-- Right (SingleType "Orange")
--
-- >>> parse dataTypeType "(test)" "[]\n"
-- Left "(test)" (line 1, column 1):
-- unexpected "["
-- expecting "ListOf", space, letter or digit or "_"
--
dataTypeType :: CharParser () DataTypeType
dataTypeType =
        try ((ListType . pack)
    <$> ((string "ListOf" *> skipSpaces) *> stringWithUnderscores))
    <|> (SingleType . pack) <$> (skipSpaces *> stringWithUnderscores)

-- | Parse a resource member
--
-- >>> parse resourceMember "(test)" "Person"
-- Right (Member (DataTypeName "Person"))
--
-- >>> parse resourceMember "(test)" "ListOf Person"
-- Right (ListMember (DataTypeName "Person"))
--
-- >>> parse resourceMember "(test)" "  \nPerson  \n  "
-- Right (Member (DataTypeName "Person"))
--
-- >>> parse resourceMember "(test)" "ListOf  \n     Person  \n  "
-- Right (ListMember (DataTypeName "Person"))
--
resourceMember :: CharParser () ResourceMember
resourceMember = try (
        ListMember
    <$> ((string "ListOf" *> skipSpaces) *> dataTypeName))
    <|> Member <$> (skipSpaces *> dataTypeName)

-- | Parse the name of a resource
--
-- >>> parse resourceName "(test)" "SomeName"
-- Right (ResourceName "SomeName")
--
-- >>> parse resourceName "(test)" "  \nSomeName   "
-- Right (ResourceName "SomeName")
--
resourceName :: CharParser () ResourceName
resourceName =
        (ResourceName . pack)
    <$> (skipSpaces *> stringWithUnderscores)

-- | Parses a Resource Type
--
-- >>> parse resource "(test)" "Person [Name, Age]"
-- Right (Resource (ResourceName "Person") [Member (DataTypeName "Name"),Member (DataTypeName "Age")])
--
-- >>> parse resource "(test)" "[Name, Age]"
-- Left "(test)" (line 1, column 1):
-- unexpected "["
-- expecting space, letter or digit or "_"
--
resource :: CharParser () Resource
resource =
        Resource
    <$> resourceName
    <*> (skipSpaces *> listOf resourceMember)

eol :: CharParser () String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
