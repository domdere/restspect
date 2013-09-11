{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RestSpec
    (   RestSpec(..)
    ,   ApiName(..)
    ,   DataTypeName(..)
    ,   DataTypeType(..)
    ,   DataType(..)
    ,   ResourceMember(..)
    ,   ResourceName(..)
    ,   Resource(..)
    ) where

import Data.Text

newtype ApiName = ApiName Text deriving (Show, Read, Eq, Ord)

newtype DataTypeName = DataTypeName Text deriving (Show, Read, Eq, Ord)

data DataTypeType = SingleType Text | ListType Text deriving (Show, Read, Eq, Ord)

data DataType = DataType DataTypeName DataTypeType deriving (Show, Read, Eq, Ord)

data ResourceMember = Member DataTypeName | ListMember DataTypeName deriving (Show, Read, Eq, Ord)

newtype ResourceName = ResourceName Text deriving (Show, Read, Eq, Ord)

data Resource = Resource ResourceName [ResourceMember] deriving (Show, Read, Eq, Ord)

data RestSpec = RestSpec
    {   apiName :: ApiName
    ,   dataTypes :: [DataType]
    } deriving (Show, Eq)
