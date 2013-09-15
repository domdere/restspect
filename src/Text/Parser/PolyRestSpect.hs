module Text.Parser.PolyRestSpect
    (   restFile
    ) where

import Control.Applicative
import Control.Monad
import Data.Text
import Text.ParserCombinators.Poly.Text hiding ( many, optional, (<|>) )

import Data.RestSpec
import Text.Parser.RestSpect.Expr

restFile :: Parser RestSpec
restFile = error "TODO"
