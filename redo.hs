import Control.Monad (filterM, liftM)
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)
import System.Environment (getArgs)

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
    Just p -> do
      (_, _, _, ph) <- createProcess $ shell $ unwords ["sh ", p, "0", takeBaseName target, tmp, " > ", tmp]
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
