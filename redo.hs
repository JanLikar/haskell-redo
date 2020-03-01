import Control.Monad (filterM, liftM)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import System.Environment (getArgs, getEnvironment)

main :: IO ()
main = do
  args <- getArgs

  _ <- mapM redo args

  return ()


redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  path <- scriptPath target

  case path of
    Nothing -> hPutStrLn stderr ("No .do file for target " ++ target)
    Just path' -> do
      let cmd = unwords ["sh ", path', "0", takeBaseName target, tmp, " > ", tmp]
      current_env <- getEnvironment
      let newEnv = toList (adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target (fromList current_env))

      (_, _, _, ph) <- createProcess $ (shell cmd) { env = Just(newEnv) }
      exitCode <- waitForProcess ph

      case exitCode of
        ExitSuccess -> do renameFile tmp target
        ExitFailure _ -> do
          hPutStrLn stderr "Redo script exited with a non-zero exit code."
          removeFile tmp


scriptPath :: FilePath -> IO (Maybe FilePath)
scriptPath target =
  listToMaybe `liftM` filterM doesFileExist candidates
  where
    candidates = [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []
