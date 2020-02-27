import System.Process

main = do
   createProcess $ shell "sh redo.do" 
