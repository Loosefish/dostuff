{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where
import Control.Monad (unless)
import Data.List (nub, inits, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (getCurrentDirectory, setCurrentDirectory, findFiles)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitSuccess, exitWith, die)
import System.FilePath (joinPath, splitPath, takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (system)


data Format = Shell
    deriving (Show, Eq)

data Dofile = Dofile
    { dFile :: FilePath
    , dFormat :: Format
    , dFunctions :: [Dofunction]
    } deriving (Show, Eq)

type Dofunction = (String, Bool)


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
    mapM_ (putStrLn . fst) functions


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
    functions <- mapMaybe (extractFunc format) . lines <$> readFile file
    return $ Dofile file format functions
  where
    format = Shell


extractFunc :: Format -> String -> Maybe Dofunction
extractFunc Shell line = if suffix `isPrefixOf` rest
    then (\n -> (n, local)) <$> stripPrefix prefix name
    else Nothing
  where
    (prefix, suffix) = ("do_", "(){")
    (name, rest) =  break (== head suffix) $ normal line
    local = "#do_local" `isSuffixOf` rest
    normal = filter (/= ' ') 


execute :: String -> [String] -> IO ()
execute func funcArgs = do
    funcs <- mapMaybe ourFunc <$> dofiles
    case funcs of
         ((dofile, f) : _) -> call dofile f funcArgs
         [] -> die $ "No such function: " ++ func
  where
    ourFunc d = (\l -> (d, (func, l))) <$> lookup func (dFunctions d)


call :: Dofile -> Dofunction -> [String] -> IO ()
call (Dofile file Shell _) (func, local) args = do
    unless local (setCurrentDirectory $ takeDirectory file)
    system (unwords ["source", file, "&&", funcName Shell func, unwords args]) >>= exitWith


funcName :: Format -> String -> String
funcName Shell func = "do_" ++ func
