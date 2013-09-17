module Text.Parser.PolyRestSpect
    (   restFile
    ) where

import Control.Monad
import Data.Text
import Text.Parse

import Data.RestSpec
import Text.Parser.RestSpect.Expr

restFile :: Parser RestSpec
restFile = error "TODO"

-- | A Parser For a DataNameExpr
dataNameExpr :: Parser DataNameExpr
dataNameExpr = DataNameExpr `fmap` many alphanum
