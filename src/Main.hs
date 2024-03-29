module Main
  ( main
  )
where
import Data.List (nub, inits, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (getCurrentDirectory, findFiles)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitWith, die, ExitCode(..))
import System.FilePath (joinPath, splitPath, takeDirectory)
import System.IO (hPutStr, stderr)
import qualified System.Process as P


data Mode = ShowUsage | ShowFiles | ShowFunctions | Execute Function Bool
          deriving Show
type Function = (String, [String])


main :: IO ()
main = do
  mode <- parseArgs <$> getArgs
  case mode of
    Nothing -> die "Illegal argument(s)."

    Just ShowUsage -> usage

    Just ShowFiles -> putStr =<< unlines <$> dofiles

    Just ShowFunctions -> putStr =<< unlines <$> (allFuncNames =<< dofiles)

    Just (Execute (name, args) local) -> do
      file <- funcFile name
      case file of
        Nothing -> die $ "Unknown function: \"" ++ name ++ "\"."
        Just f -> execute f name args local


execute :: FilePath -> String -> [String] -> Bool -> IO ()
execute file name args local = do
  cwd <- getCurrentDirectory
  let setWd = if local then cwd else takeDirectory file
  env <- getEnvironment
  (_, _, _, processH) <- P.createProcess $ process
    { P.cwd = Just setWd
    , P.env = Just (env ++ [("DOSTUFF_CWD", cwd)])
    }
  exitWith =<< P.waitForProcess processH
  where
    process = P.proc "bash" $ ["-c", script, "dostuff", file, name] ++ args
    script
      = "_dofile=\"$1\" && _dofunc=\"$2\" && shift 2 && source \"${_dofile}\" && do_${_dofunc} \"$@\""


parseArgs :: [String] -> Maybe Mode
parseArgs ("-h" : _) = Just ShowUsage
parseArgs ("-f" : _) = Just ShowFiles
parseArgs ("-p" : _) = Just ShowFunctions
parseArgs ("-l" : rest) = case parseArgs rest of
  Just (Execute f _) -> Just $ Execute f True
  _ -> Nothing
parseArgs (f : fArgs) = Just $ Execute (f, fArgs) False
parseArgs [] = Just $ Execute ("", []) False


usage :: IO ()
usage = hPutStr stderr =<< unlines . text <$> getProgName
  where
    text prog =
      [ "Usage: " ++ prog ++ " [-f | -p | [-l] FUNCTION [ARG1 ...]]"
      , "  -f                   Print paths of available dofiles"
      , "  -p                   Print names of available functions"
      , "  -l                   Force execution in current working directory"
      , "  FUNCTION [ARG1 ...]  Call local function with optional arguments"
      ]


dofiles :: IO [FilePath]
dofiles = do
  filename <- fromMaybe "Dofile" <$> lookupEnv "DOFILE"
  paths <- parentPaths <$> getCurrentDirectory
  findFiles paths filename
  where parentPaths = map joinPath . reverse . drop 1 . inits . splitPath


allFuncNames :: [FilePath] -> IO [String]
allFuncNames paths = nub . concat <$> mapM funcNames paths


funcNames :: FilePath -> IO [String]
funcNames path = do
  (code, out, err) <- P.readCreateProcessWithExitCode prog ""
  case code of
    ExitFailure _ -> die $ "Error processing dofile '" ++ path ++ "'\n" ++ err
    _ -> return $ mapMaybe (stripPrefix "declare -f do_") $ lines out
  where
    prog =
      P.proc "bash" ["-c", "source \"$1\" && declare -F", "funcNames", path]


funcFile :: String -> IO (Maybe FilePath)
funcFile name = funcFile' =<< dofiles
  where
    funcFile' [] = return Nothing
    funcFile' (f : fs) = do
      funcs <- funcNames f
      if name `elem` funcs then return $ Just f else funcFile' fs
