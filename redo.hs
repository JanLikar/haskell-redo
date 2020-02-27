import System.Directory (removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  mapM redo args

  putStrLn "Done"
  return ()


redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  (_, _, _, ph) <- createProcess $ shell ("sh " ++ target ++ ".do - - " ++ tmp ++ " > " ++ tmp)
  exitCode <- waitForProcess ph

  case exitCode of
    ExitSuccess -> do renameFile tmp target
    ExitFailure e -> do
      hPutStrLn stderr "Redo script exited with a non-zero exit code."
      removeFile tmp

  return ()