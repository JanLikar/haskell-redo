{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException)
import Control.Monad (filterM, liftM)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import GHC.IO.Exception (IOErrorType(InappropriateType))
import System.Directory (doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import System.Environment (getArgs, getEnvironment)

main :: IO ()
main = do
  args <- getArgs

  mapM_ redo args


redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target

  if upToDate' 
    then return ()
  else do
    let tmp = target ++ "---redoing"
    path <- scriptPath target

    case path of
      Nothing -> hPutStrLn stderr ("No .do file for target " ++ target)
      Just path' -> do
        let cmd = unwords ["sh ", path', "0", takeBaseName target, tmp, " > ", tmp]
        current_env <- getEnvironment
        let newEnv = toList (adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target (fromList current_env))

        (_, _, _, ph) <- createProcess $ (shell cmd) { env = Just newEnv }
        exitCode <- waitForProcess ph

        case exitCode of
          ExitSuccess -> renameFile tmp target
          ExitFailure _ -> do
            hPutStrLn stderr "Redo script exited with a non-zero exit code."
            removeFile tmp


scriptPath :: FilePath -> IO (Maybe FilePath)
scriptPath target =
    listToMaybe `fmap` filterM doesFileExist candidates
  where
    candidates = (target ++ ".do") : [replaceBaseName target "default.do" | hasExtension target]

upToDate :: String -> IO Bool
upToDate target = catch
    (do deps <- getDirectoryContents depDir
        and `fmap` mapM depUpToDate (filter isFile deps))
    (\(e :: IOException) -> return False)
  where
    depDir = ".redo" </> target

    isFile :: FilePath -> Bool
    isFile "." = False
    isFile ".." = False
    isFile _ = True

    depUpToDate :: FilePath -> IO Bool
    depUpToDate dep = catch
      (do oldHash <- readFile $ depDir </> dep
          hPutStrLn stderr oldHash
          return False)
      (\e -> return (ioeGetErrorType e == InappropriateType))
