import System.Process (createProcess, waitForProcess,shell)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  mapM redo args

  putStrLn "Done"
  return ()


redo :: String -> IO ()
redo target = do
  (_, _, _, ph) <- createProcess $ shell ("sh " ++ target ++ ".do")
  waitForProcess ph
  return ()