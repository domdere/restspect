{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RestSpec
    (   RestSpec(..)
    ,   ApiName(..)
    ) where

import Data.Text

newtype ApiName = ApiName Text deriving (Show, Read, Eq, Ord)

data RestSpec = RestSpec
    {   apiName :: ApiName
    } deriving (Show, Eq)

