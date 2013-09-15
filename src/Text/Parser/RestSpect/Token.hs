module Text.Parser.RestSpect.Token where

data RestToken =
        StringToken String
    |   DataTypeToken
    |   ServesToken
    |   ColonToken
    |   TickToken deriving (Show, Eq)
