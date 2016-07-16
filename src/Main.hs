{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where
import Control.Monad 
import Data.List (inits, unwords)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process (callCommand)


data Dofile = Dofile FilePath Format
    deriving (Show)

data Format = Shell
    deriving (Show)


main :: IO ()
main = do
    args <- getArgs
    case args of
         ["-h"] -> usage
         ["--help"] -> usage
         ["-f"] -> printDofiles
         (func : funcArgs) -> run func funcArgs
         [] -> run "" []
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
printDofiles = do
    files <- dofiles
    let fileNames = map (\(Dofile f _) -> f) files
    mapM_ putStrLn fileNames


run :: String -> [String] -> IO ()
run func funcArgs = do
    funcFile <- fmap listToMaybe (filterM (`provides` func) =<< dofiles)
    case funcFile of
         Just file -> execute file func funcArgs
         Nothing -> die $ "No such function: " ++ func


dofiles :: IO [Dofile]
dofiles = do
    filename <- fromMaybe "Dofile" <$> lookupEnv "DOFILE"
    here <- getCurrentDirectory
    let paths = map joinPath $ reverse $ drop 1 $ inits $ splitPath here
    map toDofile <$> findFiles paths filename


toDofile :: FilePath -> Dofile
toDofile file = Dofile file Shell


provides :: Dofile -> String -> IO Bool
provides (Dofile file Shell) func =
    any (T.isPrefixOf funcFull) <$> normalLines
  where
    normalLines = map (T.replace " " "") . T.lines <$> T.readFile file
    funcFull = T.concat ["do_", T.pack func, "(){"]


execute :: Dofile -> String -> [String] -> IO ()
execute (Dofile file Shell) func args = callCommand cmd
  where
    cmd = "source " ++ file ++ " && do_" ++ func ++ " " ++ unwords args
