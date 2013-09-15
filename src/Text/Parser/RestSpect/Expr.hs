{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Parser.RestSpect.Expr where

data ServiceExpr = ServiceExpr String [SpecExpr] deriving (Show, Eq)

data SpecExpr =
        DataType String RawTypeExpr
    |   ResourceType ResourceExpr
    |   URIMethod URIMethodExpr deriving (Show, Eq)

data ResourceExpr = ResourceExpr String ResourceSpecExpr deriving (Show, Eq)

data ResourceSpecExpr = ResourceSpecExpr [DataNameExpr] deriving (Show, Eq)

data DerivedResourceSpecExpr =
        Named DataNameExpr
    |   Anonymous ResourceSpecExpr
    |   ListOf DerivedResourceSpecExpr deriving (Show, Eq)

newtype DataNameExpr = DataNameExpr String deriving (Show, Eq)

data DataTypeExpr = DataTypeExpr DataNameExpr RawTypeExpr deriving (Show, Eq)

data RawTypeExpr =
        RawInt
    |   RawFloat
    |   RawDateTime
    |   RawText
    |   RawList RawTypeExpr deriving (Show, Eq)

data URIMethodExpr = URIMethodExpr MethodExpr URIExpr [URIPropertyExpr] deriving (Show, Eq)

data MethodExpr =
        Get
    |   Post
    |   Put
    |   Delete deriving (Show, Eq)

newtype URIExpr = URIExpr String deriving (Show, Eq)

data FormatExpr =
        Json
    |   Xml
    |   Bson deriving (Show, Eq)

newtype DescriptionExpr = DescriptionExpr String deriving (Show, Eq)

data RepresentationExpr =
        None
    |   RepresentationExpr ResourceSpecExpr FormatExpr deriving (Show, Eq)

newtype ParameterNameExpr = ParameterNameExpr String deriving (Show, Eq)

newtype ParameterDescExpr = ParameterDescExpr String deriving (Show, Eq)

data ParameterExpr = ParameterExpr ParameterNameExpr ParameterDescExpr deriving (Show, Eq)

newtype ErrorCodeExpr = ErrorCodeExpr Int deriving (Show, Eq, Num, Ord)

newtype ErrorDescExpr = ErrorDescExpr String deriving (Show, Eq)

data ErrorExpr = ErrorExpr ErrorCodeExpr ErrorDescExpr deriving (Show, Eq)

newtype NoteExpr = NoteExpr String deriving (Show, Eq)

data URIPropertyExpr =
        Return RepresentationExpr
    |   Description DescriptionExpr
    |   Parameters [ParameterExpr]
    |   Body RepresentationExpr
    |   Errors [ErrorExpr]
    |   Notes NoteExpr deriving (Show, Eq)
