{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process (system)


data Format = Shell
    deriving (Show, Eq)

data Dofile = Dofile
    { dFile :: FilePath
    , dFormat :: Format
    , dFunctions :: [String]
    } deriving (Show, Eq)



main :: IO ()
main = do
    args <- getArgs
    case args of
         ["-h"] -> usage
         ["--help"] -> usage
         ["-f"] -> printDofiles
         ["-p"] -> printFunctions
         (func : funcArgs) -> execute func funcArgs
         [] -> execute "" []
    exitSuccess


usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " [-f | -p | [-l] FUNCTION [ARG1 ...]]"
    hPutStrLn stderr "  -f                   Print paths of available dofiles"
    hPutStrLn stderr "  -p                   Print names of available functions"
    hPutStrLn stderr "  -l                   Force execution in current working directory"
    hPutStrLn stderr "  FUNCTION [ARG1 ...]  Call local function with optional arguments"


printDofiles :: IO ()
printDofiles = mapM_ (putStrLn . dFile) =<< dofiles


printFunctions :: IO ()
printFunctions = do
    functions <- nub . concatMap dFunctions <$> dofiles
    mapM_ putStrLn functions


dofiles :: IO [Dofile]
dofiles = do
    filename <- fromMaybe "Dofile" <$> lookupEnv "DOFILE"
    paths <- parentPaths <$> getCurrentDirectory
    files <- findFiles paths filename
    mapM toDofile files
  where
    parentPaths = map joinPath . reverse . drop 1 . inits . splitPath


toDofile :: FilePath -> IO Dofile
toDofile file = do
    functions <- mapMaybe (extractName format) . lines <$> readFile file
    return $ Dofile file format functions
  where
    format = Shell


extractName :: Format -> String -> Maybe String
extractName Shell line = if suffix `isPrefixOf` rest
        then stripPrefix prefix name
        else Nothing
  where
    (prefix, suffix) = ("do_", "(){")
    (name, rest) =  break (== head suffix) $ normal line
    normal = filter (/= ' ') 


execute :: String -> [String] -> IO ()
execute func funcArgs = do
    does <- filter providesFunc <$> dofiles
    case does of
         (dofile : _) -> call dofile func funcArgs
         [] -> die $ "No such function: " ++ func
  where
    providesFunc d = func `elem` dFunctions d


call :: Dofile -> String -> [String] -> IO ()
call (Dofile file Shell _) func args = do
    setCurrentDirectory $ takeDirectory file
    system (unwords ["source", file, "&&", funcName Shell func, unwords args]) >>= exitWith


funcName :: Format -> String -> String
funcName Shell func = "do_" ++ func
