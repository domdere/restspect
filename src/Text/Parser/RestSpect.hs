module Text.Parser.RestSpect
    ( restFile
    ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Text
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Data.RestSpec
import Text.Parser.RestSpect.Expr

skipSpaces :: CharParser () (Maybe ())
skipSpaces = optional $ skipMany space

-- | parses a string with underscore characters included
--
-- >>> parse stringWithUnderscores "(test)" "blah_bl\ah"
-- Right "blah_bl"
--
-- >>> parse stringWithUnderscores "(test)" "blah_blah"
-- Right "blah_blah"
--
stringWithUnderscores :: CharParser () String
stringWithUnderscores = many1 charParser
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

-- | Parses a URI token (an alpha numeric
-- string with '/'s, '%'s, '?'s, '&'s)
-- and returns the string while dropping trailing spaces
--
-- >>> parse uriToken "(test)" "  Bad/URL"
-- Left "(test)" (line 1, column 1):
-- unexpected " "
-- expecting letter or digit, "_", "-", "/", "?", "&", "%", "{", "}" or "="
--
-- >>> parse uriToken "(test)" "Good/url"
-- Right "Good/url"
--
-- >>> parse uriToken "(test)" "Good/Name/With/Trailing/%20Spaces?x=1&y=2  \n  "
-- Right "Good/Name/With/Trailing/%20Spaces?x=1&y=2"
--
uriToken :: CharParser () String
uriToken = many1 uriCharParser <* skipSpaces
    where
        uriCharParser :: CharParser () Char
        uriCharParser =
                try alphaNum
            <|> try (char '_')
            <|> try (char '-')
            <|> try (char '/')
            <|> try (char '?')
            <|> try (char '&')
            <|> try (char '%')
            <|> try (char '{')
            <|> try (char '}')
            <|> char '='

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
-- expecting letter or digit, "_", "-", "/", "?", "&", "%", "{", "}" or "="
--
-- >>> parse uriExpr "(test)" "GoodURI"
-- Right (URIExpr "GoodURI")
--
-- >>> parse uriExpr "(test)" "GoodURIWithTrailingSpaces  \n  "
-- Right (URIExpr "GoodURIWithTrailingSpaces")
--
uriExpr :: CharParser () URIExpr
uriExpr = URIExpr <$> uriToken

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

-- | Parses a restFile
restFile :: CharParser () ServiceExpr
restFile = skipSpaces *> serviceExpr

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
    (   try (DataType <$> dataTypeExpr)
    <|> try (ResourceType <$> resourceExpr)
    <|> (URIMethod <$> uriMethodExpr)
    ) <* skipSpaces

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
    (   try (ListOf <$> (between `on` char) '[' ']' derivedResourceSpecExpr)
    <|> try (Anonymous <$> resourceSpecExpr)
    <|> Named <$> dataNameExpr
    ) <* skipSpaces

-- | Parses a URIMethodExpr
--
-- >>> parse uriMethodExpr "(test)" "GET /persons/: Returns: Person in JSON end"
-- Right (URIMethodExpr Get (URIExpr "/persons/") [Return (RepresentationExpr (Named (DataNameExpr "Person")) Json)])
--
uriMethodExpr :: CharParser () URIMethodExpr
uriMethodExpr =
        URIMethodExpr
    <$> methodExpr
    <*> (uriExpr <* char ':' <* skipSpaces)
    <*> uriPropertyExpr `manyTill` string "end"

-- | Parses a MethodExpr
--
-- >>> parse methodExpr "(test)" "invalid"
-- Left "(test)" (line 1, column 1):
-- unexpected "i"
-- expecting "GET", "POST", "PUT" or "DELETE"
--
-- >>> parse methodExpr "(test)" "GET"
-- Right Get
--
-- >>> parse methodExpr "(test)" "POST"
-- Right Post
--
-- >>> parse methodExpr "(test)" "PUT"
-- Right Put
--
-- >>> parse methodExpr "(test)" "DELETE"
-- Right Delete
--
methodExpr :: CharParser () MethodExpr
methodExpr =
    (   try (string "GET" *> return Get)
    <|> try (string "POST" *> return Post)
    <|> try (string "PUT" *> return Put)
    <|> string "DELETE" *> return Delete
    ) <* skipSpaces

-- | Parses a URIPropertyExpr
--
-- >>> parse uriPropertyExpr "(test)" "Returns: Something in XML"
-- Right (Return (RepresentationExpr (Named (DataNameExpr "Something")) Xml))
--
-- >>> parse uriPropertyExpr "(test)" "Description:   \n  \"A Description\"  "
-- Right (Description (DescriptionExpr "A Description"))
--
-- >>> parse uriPropertyExpr "(test)" "Parameters: [param1: \"The first parameter\" \n,   param2 : \"The second parameter\"]"
-- Right (Parameters [ParameterExpr (ParameterNameExpr "param1") (ParameterDescExpr "The first parameter"),ParameterExpr (ParameterNameExpr "param2") (ParameterDescExpr "The second parameter")])
--
-- >>> parse uriPropertyExpr "(test)" "Body: SomethingElse in JSON"
-- Right (Body (RepresentationExpr (Named (DataNameExpr "SomethingElse")) Json))
--
-- >>> parse uriPropertyExpr "(test)" "Errors: [404: \"Not Found.\"]"
-- Right (Errors [ErrorExpr (ErrorCodeExpr 404) (ErrorDescExpr "Not Found.")])
--
-- >>> parse uriPropertyExpr "(test)" "Notes: \"Something Strange\""
-- Right (Notes (NoteExpr "Something Strange"))
--
uriPropertyExpr :: CharParser () URIPropertyExpr
uriPropertyExpr =
    (   try returnLine
    <|> try descriptionLine
    <|> try parametersLine
    <|> try bodyLine
    <|> try errorsLine
    <|> notesLine
    ) <* skipSpaces

-- | Parses a Return Expr
--
-- >>> parse returnLine "(test)" "Returns: Something in XML"
-- Right (Return (RepresentationExpr (Named (DataNameExpr "Something")) Xml))
--
returnLine :: CharParser () URIPropertyExpr
returnLine = Return <$>
    (   string "Returns"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  representationExpr)

-- | Parses a Description URIPropertyExpr
--
-- >>> parse descriptionLine "(test)" "Description:   \n  \"A Description\"  "
-- Right (Description (DescriptionExpr "A Description"))
--
descriptionLine :: CharParser () URIPropertyExpr
descriptionLine = Description <$>
    (   string "Description"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  descriptionExpr
    )

-- | Parses a parameter URIPropertyExpr
--
-- >>> parse parametersLine "(test)" "Parameters: [param1: \"The first parameter\", \n   param2 : \"The second parameter\"]"
-- Right (Parameters [ParameterExpr (ParameterNameExpr "param1") (ParameterDescExpr "The first parameter"),ParameterExpr (ParameterNameExpr "param2") (ParameterDescExpr "The second parameter")])
--
parametersLine :: CharParser () URIPropertyExpr
parametersLine = Parameters <$>
    (   string "Parameters"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  (   try (string "None" *> return [])
        <|> listOf '[' ']' ',' parameterExpr
        )
    )

-- | Parses a Body URIPropertyExpr
--
-- >>> parse bodyLine "(test)" "Body: SomethingElse in JSON"
-- Right (Body (RepresentationExpr (Named (DataNameExpr "SomethingElse")) Json))
--
bodyLine :: CharParser () URIPropertyExpr
bodyLine = Body <$>
    (   string "Body"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  representationExpr)


-- | Parses an error URIPropertyExpr
--
-- >>> parse errorsLine "(test)" "Errors: [404: \"The first error\" \n,   401 : \"The other error\"]"
-- Right (Errors [ErrorExpr (ErrorCodeExpr 404) (ErrorDescExpr "The first error"),ErrorExpr (ErrorCodeExpr 401) (ErrorDescExpr "The other error")])
--
errorsLine :: CharParser () URIPropertyExpr
errorsLine = Errors <$>
    (   string "Errors"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  (   try (string "None" *> return [])
        <|> listOf '[' ']' ',' errorExpr
        )
    )

-- | Parses a Notes Line
--
-- >>> parse notesLine "(test)" "Notes: \"Something Strange\""
-- Right (Notes (NoteExpr "Something Strange"))
--
notesLine :: CharParser () URIPropertyExpr
notesLine = Notes <$>
    (   string "Notes"
    *>  skipSpaces
    *>  char ':'
    *>  skipSpaces
    *>  noteExpr
    )

-- | Parses a RepresentationExpr
--
representationExpr :: CharParser () RepresentationExpr
representationExpr =
        try (string "Nothing" *> return None)
    <|> (   RepresentationExpr
        <$> derivedResourceSpecExpr
        <*> formatExpr)

-- | Parses a FormatExpr
--
-- >>> parse formatExpr "(test)" "in JSON"
-- Right Json
--
-- >>> parse formatExpr "(test)" "in XML"
-- Right Xml
--
-- >>> parse formatExpr "(test)" "in BSON"
-- Right Bson
--
formatExpr :: CharParser () FormatExpr
formatExpr = string "in" *> skipSpaces *>
    (   try (string "JSON" *> return Json)
    <|> try (string "XML" *> return Xml)
    <|> try (string "BSON" *> return Bson)
    ) <* skipSpaces

-- | Parses a ParameterExpr
--
-- >>> parse parameterExpr "(test)" "id : \"The ID for the resource\""
-- Right (ParameterExpr (ParameterNameExpr "id") (ParameterDescExpr "The ID for the resource"))
--
parameterExpr :: CharParser () ParameterExpr
parameterExpr =
    (   ParameterExpr
    <$> (   parameterNameExpr
        <*  skipSpaces
        <*  char ':'
        <*  skipSpaces
        )
    <*> parameterDescExpr
    ) <* skipSpaces

-- | parses an ErrorExpr
--
-- >>> parse errorExpr "(test)" "404 : \"not found\""
-- Right (ErrorExpr (ErrorCodeExpr 404) (ErrorDescExpr "not found"))
--
errorExpr :: CharParser () ErrorExpr
errorExpr =
    (   ErrorExpr
    <$> (   errorCodeExpr
        <*  skipSpaces
        <*  char ':'
        <*  skipSpaces
        )
    <*> errorDescExpr
    ) <* skipSpaces

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
