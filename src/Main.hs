{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where
import Control.Monad 
import Data.List
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process (callCommand)


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
printDofiles = do
    files <- dofiles
    mapM_ (putStrLn .dFile) files


dofiles :: IO [Dofile]
dofiles = do
    filename <- fromMaybe "Dofile" <$> lookupEnv "DOFILE"
    here <- getCurrentDirectory
    let paths = map joinPath $ reverse $ drop 1 $ inits $ splitPath here
    files <- findFiles paths filename
    mapM toDofile files


toDofile :: FilePath -> IO Dofile
toDofile file = do
    normalLines <- map (filter (/= ' ')) . lines <$> readFile file
    return $ Dofile file Shell $ map (takeWhile (/= '(')) $ filter isFunc normalLines
  where
    isFunc l = isPrefixOf "do_" l && isPrefixOf "(){" (dropWhile (/= '(') l)


execute :: String -> [String] -> IO ()
execute func funcArgs = do
    files <- dofiles
    case filter (\d -> func' `elem` dFunctions d) files of
         (file : _) -> call file func funcArgs
         [] -> die $ "No such function: " ++ func
  where
    func' = "do_" ++ func


call :: Dofile -> String -> [String] -> IO ()
call (Dofile file Shell _) func args =
    callCommand $ unwords ["source", file, "&&", "do_" ++ func, unwords args]
