{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RestSpec
    (   RestSpec(..)
    ,   ApiName(..)
    ,   DataTypeName(..)
    ,   DataTypeType(..)
    ,   DataType(..)
    ) where

import Data.Text

newtype ApiName = ApiName Text deriving (Show, Read, Eq, Ord)

newtype DataTypeName = DataTypeName Text deriving (Show, Read, Eq, Ord)
data DataTypeType = SingleType Text | ListType Text deriving (Show, Read, Eq, Ord)

data DataType = DataType DataTypeName DataTypeType deriving (Show, Read, Eq, Ord)

data RestSpec = RestSpec
    {   apiName :: ApiName
    ,   dataTypes :: [DataType]
    } deriving (Show, Eq)

