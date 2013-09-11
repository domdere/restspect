module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )

import Text.Parser.RestSpect

data CommandLineOption = Help | Version deriving (Show, Eq)

coreOptions :: [OptDescr CommandLineOption]
coreOptions = 
    [   Option "h?" ["help"]    (NoArg Help)    "print this usage message"
    ,   Option "V"  ["version"] (NoArg Version) "output the version"
    ]

argOrder :: ArgOrder a
argOrder = Permute

-- Define the additional options for your app here...
options :: [OptDescr CommandLineOption]
options = []

usageString :: String
usageString = "Usage: restdoc [OPTIONS] filename"

versionString :: String
versionString = "restdoc: 0.0.1"

usageMsg :: String
usageMsg = usageInfo usageString (coreOptions ++ options)

-- | prints the usage message
printUsageMsg :: IO ()
printUsageMsg = putStrLn $ usageInfo usageString (coreOptions ++ options)

-- | checks the core flags of the app and if help and version dont appear passes control onto appMain where
-- | the user can do their own opt checks
checkCoreFlagsAndRunMain :: [CommandLineOption] -> [String] -> IO ()
checkCoreFlagsAndRunMain opts args
    | Help `elem` opts      = printUsageMsg
    | Version `elem` opts   = putStrLn versionString
    | otherwise             = appMain opts args

main :: IO ()
main = do
    (opts, args, errorMsgs) <- getOpt argOrder (coreOptions ++ options) `fmap` getArgs
    if null errorMsgs then
        checkCoreFlagsAndRunMain opts args
    else
        ioError $ userError $ concat errorMsgs ++ usageMsg

appMain :: [a] -> [String] -> IO ()
appMain _ args
    | length args /= 1 = do
        putStrLn $ "Error: Expected 1 argument, got " ++ show (length args) ++ "instead"
        printUsageMsg
    | otherwise = do
        -- for now all this does is load up the .rest file and try to parse it,
        -- Will do the rest once the parsing is finished..
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        either print print $ parse restFile (head args) contents
