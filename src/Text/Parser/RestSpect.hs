module Text.Parser.RestSpect where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec
import Text.Parser.RestSpect.Expr

skipSpaces :: CharParser () (Maybe ())
skipSpaces = optional $ skipMany space

stringWithUnderscores :: CharParser () String
stringWithUnderscores = (:) <$> charParser <*> many charParser
    where
        charParser = alphaNum <|> char '_'

anythingBetween :: Char -> CharParser () String
anythingBetween c = (between `on` char) c c $ many (notFollowedBy (char c) *> anyChar)

-- | Parses a String Literal
--
-- >>> parse stringLiteral "(test)" "  \n  BadLiteral"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse stringLiteral "(test)" " \n  'BadLiteral'"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse stringLiteral "(test)" "\"GoodLiteral\""
-- Right "GoodLiteral"
--
-- >>> parse stringLiteral "(test)" "\"GoodLiteralWithTrailingSpaces\"  \n  "
-- Right "GoodLiteralWithTrailingSpaces"
--
-- >>> parse stringLiteral "(test)" "'GoodLiteralWithTrailingSpaces  \n '  AdditionalGuff"
-- Right "GoodLiteralWithTrailingSpaces  \n "
--
stringLiteral :: CharParser () String
stringLiteral =
    (   try (anythingBetween '\"')
    <|> anythingBetween '\''
    ) <* skipSpaces

-- | Parses an alpha numeric string token (with underscores)
-- and returns the string while dropping trailing spaces
--
-- >>> parse stringToken "(test)" "  BadName"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting letter or digit or "_"
--
-- >>> parse stringToken "(test)" "GoodName"
-- Right "GoodName"
--
-- >>> parse stringToken "(test)" "GoodNameWithTrailingSpaces  \n  "
-- Right "GoodNameWithTrailingSpaces"
--
stringToken :: CharParser () String
stringToken = stringWithUnderscores <* skipSpaces

-- | parses a list of things enclosed by open and close and
-- seperated by sep
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[Dom, Jot]"
-- Right ["Dom","Jot"]
--
-- >>> parse (listOf '{' '}' '|' stringToken) "(test)" "{Dom | Jot}"
-- Right ["Dom","Jot"]
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "{Dom | Jot}"
-- Left "(test)" (line 1, column 1):
-- unexpected "{"
-- expecting "["
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[Dom | Jot}"
-- Left "(test)" (line 1, column 6):
-- unexpected "|"
-- expecting space, "," or "]"
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[| Jot}"
-- Left "(test)" (line 1, column 2):
-- unexpected "|"
-- expecting space, letter or digit, "_" or "]"
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[Dom, Jot}"
-- Left "(test)" (line 1, column 10):
-- unexpected "}"
-- expecting "," or "]"
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[, Dom]"
-- Left "(test)" (line 1, column 2):
-- unexpected ","
-- expecting space, letter or digit, "_" or "]"
--
-- >>> parse (listOf '[' ']' ',' stringToken) "(test)" "[Dom,]"
-- Left "(test)" (line 1, column 6):
-- unexpected "]"
-- expecting space, letter or digit or "_"
--
listOf :: Char -> Char -> Char -> CharParser () a -> CharParser () [a]
listOf open close sep p =
        (between `on` char) open close
    (   skipSpaces
    *>  ((p <* skipSpaces) `sepBy` (char sep <* skipSpaces))
    ) <* skipSpaces


precedes :: String -> CharParser () a -> CharParser () a
precedes str parser =
        (string str <* skipSpaces)
    *>  (skipSpaces *> parser)

-- | Parses a DataName expression
--
-- >>> parse dataNameExpr "(test)" "  BadName"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting letter or digit or "_"
--
-- >>> parse dataNameExpr "(test)" "GoodName"
-- Right (DataNameExpr "GoodName")
--
-- >>> parse dataNameExpr "(test)" "GoodNameWithTrailingSpaces  \n  "
-- Right (DataNameExpr "GoodNameWithTrailingSpaces")
--
dataNameExpr :: CharParser () DataNameExpr
dataNameExpr = DataNameExpr <$> stringToken

-- | Parses a URI expression
--
-- >>> parse uriExpr "(test)" "  BadURI"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting letter or digit or "_"
--
-- >>> parse uriExpr "(test)" "GoodURI"
-- Right (URIExpr "GoodURI")
--
-- >>> parse uriExpr "(test)" "GoodURIWithTrailingSpaces  \n  "
-- Right (URIExpr "GoodURIWithTrailingSpaces")
--
uriExpr :: CharParser () URIExpr
uriExpr = URIExpr <$> stringToken

-- | Parses a Description expression
--
-- >>> parse descriptionExpr "(test)" "  \n  BadDescription"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse descriptionExpr "(test)" "'GoodDescription'"
-- Right (DescriptionExpr "GoodDescription")
--
-- >>> parse descriptionExpr "(test)" "'GoodDescriptionWithTrailingSpaces'  \n  "
-- Right (DescriptionExpr "GoodDescriptionWithTrailingSpaces")
--
descriptionExpr :: CharParser () DescriptionExpr
descriptionExpr = DescriptionExpr <$> stringLiteral

-- | Parses a Parameter Name Expression
--
-- >>> parse parameterNameExpr "(test)" "  \n  BadParameter"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting letter or digit or "_"
--
-- >>> parse parameterNameExpr "(test)" "GoodParameterName"
-- Right (ParameterNameExpr "GoodParameterName")
--
-- >>> parse parameterNameExpr "(test)" "GoodParameterNameWithTrailingSpaces  \n  "
-- Right (ParameterNameExpr "GoodParameterNameWithTrailingSpaces")
--
-- >>> parse parameterNameExpr "(test)" "GoodParameterNameWithTrailingSpaces  \n  AdditionalGuff"
-- Right (ParameterNameExpr "GoodParameterNameWithTrailingSpaces")
--
parameterNameExpr :: CharParser () ParameterNameExpr
parameterNameExpr =
        ParameterNameExpr
    <$> stringToken

-- | Parses a Parameter Description Expression
--
-- >>> parse parameterDescExpr "(test)" "  \n  BadParameterDesc"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse parameterDescExpr "(test)" "  \n  'BadParameterDesc'"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse parameterDescExpr "(test)" "'GoodParameterDesc'"
-- Right (ParameterDescExpr "GoodParameterDesc")
--
-- >>> parse parameterDescExpr "(test)" "'GoodParameterDescWithTrailingSpaces'  \n  "
-- Right (ParameterDescExpr "GoodParameterDescWithTrailingSpaces")
--
-- >>> parse parameterDescExpr "(test)" "'GoodParameterDescWithTrailingSpaces  \n ' AdditionalGuff"
-- Right (ParameterDescExpr "GoodParameterDescWithTrailingSpaces  \n ")
--
parameterDescExpr :: CharParser () ParameterDescExpr
parameterDescExpr = ParameterDescExpr <$> stringLiteral

-- | Parses an Error Code expression
--
-- >>> parse errorCodeExpr "(test)" "  non digit stuff"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting digit
--
-- >>> parse errorCodeExpr "(test)" "non digit stuff"
-- Left "(test)" (line 1, column 1):
-- unexpected "n"
-- expecting digit
--
-- >>> parse errorCodeExpr "(test)" "404   \n "
-- Right (ErrorCodeExpr 404)
errorCodeExpr :: CharParser () ErrorCodeExpr
errorCodeExpr =
        (ErrorCodeExpr . read)
    <$> many1 digit
    <*  skipSpaces

-- | Parses an Error Description Expression
--
-- >>> parse errorDescExpr "(test)" "  \n  BadErrorDesc"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse errorDescExpr "(test)" "  \n 'BadErrorDesc'"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse errorDescExpr "(test)" "'GoodErrorDesc'"
-- Right (ErrorDescExpr "GoodErrorDesc")
--
-- >>> parse errorDescExpr "(test)" "'GoodErrorDescWithTrailingSpaces'  \n  "
-- Right (ErrorDescExpr "GoodErrorDescWithTrailingSpaces")
--
-- >>> parse errorDescExpr "(test)" "'GoodErrorDescWithTrailingSpaces  \n '  AdditionalGuff"
-- Right (ErrorDescExpr "GoodErrorDescWithTrailingSpaces  \n ")
--
errorDescExpr :: CharParser () ErrorDescExpr
errorDescExpr = ErrorDescExpr <$> stringLiteral

-- | Parses a noteExpr
--
-- >>> parse noteExpr "(test)" "  \n  BadNote"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse noteExpr "(test)" " \n  'BadNote'"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting "\"" or "'"
--
-- >>> parse noteExpr "(test)" "\"GoodNote\""
-- Right (NoteExpr "GoodNote")
--
-- >>> parse noteExpr "(test)" "\"GoodNoteWithTrailingSpaces\"  \n  "
-- Right (NoteExpr "GoodNoteWithTrailingSpaces")
--
-- >>> parse noteExpr "(test)" "'GoodNoteWithTrailingSpaces  \n '  AdditionalGuff"
-- Right (NoteExpr "GoodNoteWithTrailingSpaces  \n ")
--
noteExpr :: CharParser () NoteExpr
noteExpr = NoteExpr <$> stringLiteral

-- | Parses a Service Expression
--
-- >>> parse serviceExpr "(test)" "service ServiceName where"
-- Right (ServiceExpr "ServiceName" [])
--
serviceExpr :: CharParser () ServiceExpr
serviceExpr =
        ServiceExpr
    <$> ((string "service" <* skipSpaces) *> stringToken)
    <*  skipSpaces
    <*> ((string "where" <* skipSpaces) *> many specExpr)

-- | Parses a Spec Expression
--
-- >>> parse specExpr "(test)" "datatype  typeName = Int"
-- Right (DataType (DataTypeExpr (DataNameExpr "typeName") RawInt))
--
specExpr :: CharParser () SpecExpr
specExpr =
        try (DataType <$> dataTypeExpr)
    <|> try (ResourceType <$> resourceExpr)
    <|> (URIMethod <$> uriMethodExpr)

-- | Parses a datatype Expression
--
-- >>> parse dataTypeExpr "(test)" "datatype  typeName = Int"
-- Right (DataTypeExpr (DataNameExpr "typeName") RawInt)
--
-- >>> parse dataTypeExpr "(test)" "datatype typeName = DateTime"
-- Right (DataTypeExpr (DataNameExpr "typeName") RawDateTime)
--
-- >>> parse dataTypeExpr "(test)" "datatype  typeName = [Text]"
-- Right (DataTypeExpr (DataNameExpr "typeName") (RawList RawText))
--
-- >>> parse dataTypeExpr "(test)" "dataype  typeName = [Text]"
-- Left "(test)" (line 1, column 1):
-- unexpected "y"
-- expecting "datatype"
--
dataTypeExpr :: CharParser () DataTypeExpr
dataTypeExpr =
        DataTypeExpr
    <$> (string "datatype" *> skipSpaces *> dataNameExpr)
    <*> (char '=' *> skipSpaces *> rawTypeExpr)

-- | Parses a Resource Name Expressions
--
resourceNameExpr :: CharParser () ResourceNameExpr
resourceNameExpr = ResourceNameExpr <$> stringToken

-- | Parses a Data Member Name Expression
--
memberNameExpr :: CharParser () MemberNameExpr
memberNameExpr = MemberNameExpr <$> stringToken

-- | Parses a Resource Expression
--
-- >>> parse resourceExpr "(test)" "resource Person = {name: Name, age: Age}"
-- Right (ResourceExpr (ResourceNameExpr "Person") (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "name") (Named (DataNameExpr "Name")),DataMemberExpr (MemberNameExpr "age") (Named (DataNameExpr "Age"))]))
--
resourceExpr :: CharParser () ResourceExpr
resourceExpr =
        ResourceExpr
    <$> (string "resource" *> skipSpaces *> resourceNameExpr)
    <*> (char '=' *> skipSpaces *> resourceSpecExpr)

-- | Parses a Resource Spec Expression
--
-- >>> parse resourceSpecExpr "(test)" "{name: Name, age: Age}"
-- Right (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "name") (Named (DataNameExpr "Name")),DataMemberExpr (MemberNameExpr "age") (Named (DataNameExpr "Age"))])
--
-- >>> parse resourceSpecExpr "(test)" "{names: [Name], age: Age}"
-- Right (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "names") (ListOf (Named (DataNameExpr "Name"))),DataMemberExpr (MemberNameExpr "age") (Named (DataNameExpr "Age"))])
--
-- >>> parse resourceSpecExpr "(test)" "{names: [Name], age: {age_rel_to_jane: Duration, janes_age: Age} }"
-- Right (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "names") (ListOf (Named (DataNameExpr "Name"))),DataMemberExpr (MemberNameExpr "age") (Anonymous (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "age_rel_to_jane") (Named (DataNameExpr "Duration")),DataMemberExpr (MemberNameExpr "janes_age") (Named (DataNameExpr "Age"))]))])
--
resourceSpecExpr :: CharParser () ResourceSpecExpr
resourceSpecExpr = ResourceSpecExpr <$> listOf '{' '}' ',' dataMemberExpr

-- | Parses a Data Member Expression
--
-- >>> parse dataMemberExpr "(test)" "name: Name"
-- Right (DataMemberExpr (MemberNameExpr "name") (Named (DataNameExpr "Name")))
--
dataMemberExpr :: CharParser () DataMemberExpr
dataMemberExpr =
        DataMemberExpr
    <$> memberNameExpr
    <*> (char ':' *> skipSpaces *> derivedResourceSpecExpr)

-- | Parses a Derived Resource Spec Expression
--
-- >>> parse derivedResourceSpecExpr "(test)" "HelloWorld"
-- Right (Named (DataNameExpr "HelloWorld"))
--
-- >>> parse derivedResourceSpecExpr "(test)" "[HelloWorld]"
-- Right (ListOf (Named (DataNameExpr "HelloWorld")))
--
-- >>> parse derivedResourceSpecExpr "(test)" "{hello: World}"
-- Right (Anonymous (ResourceSpecExpr [DataMemberExpr (MemberNameExpr "hello") (Named (DataNameExpr "World"))]))
--
derivedResourceSpecExpr :: CharParser () DerivedResourceSpecExpr
derivedResourceSpecExpr =
        try (ListOf <$> (between `on` char) '[' ']' derivedResourceSpecExpr)
    <|> try (Anonymous <$> resourceSpecExpr)
    <|> Named <$> dataNameExpr

uriMethodExpr :: CharParser () URIMethodExpr
uriMethodExpr = error "TODO"

-- | Parses a RawType Expression
--
-- >>> parse rawTypeExpr "(test)" "Int"
-- Right RawInt
--
-- >>> parse rawTypeExpr "(test)" "Float"
-- Right RawFloat
--
-- >>> parse rawTypeExpr "(test)" "DateTime"
-- Right RawDateTime
--
-- >>> parse rawTypeExpr "(test)" "Text"
-- Right RawText
--
-- >>> parse rawTypeExpr "(test)" "[Int]"
-- Right (RawList RawInt)
--
-- >>> parse rawTypeExpr "(test)" "[Float]"
-- Right (RawList RawFloat)
--
-- >>> parse rawTypeExpr "(test)" "[DateTime]"
-- Right (RawList RawDateTime)
--
-- >>> parse rawTypeExpr "(test)" "[Text]"
-- Right (RawList RawText)
--
-- >>> parse rawTypeExpr "(test)" "AnythingElse"
-- Left "(test)" (line 1, column 1):
-- unexpected "A"
-- expecting "Int", "Float", "DateTime", "Text" or "["
--
rawTypeExpr :: CharParser () RawTypeExpr
rawTypeExpr =
        try (string "Int" *> return RawInt)
    <|> try (string "Float" *> return RawFloat)
    <|> try (string "DateTime" *> return RawDateTime)
    <|> try (string "Text" *> return RawText)
    <|> RawList <$> (between `on` char) '[' ']' rawTypeExpr
