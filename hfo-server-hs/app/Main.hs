module Main where

-- import System.IO       (hGetContents, hSetBuffering, BufferMode(..))
import System.Process  
import Control.Monad
import HFO.Types

-- import System.TimeIt

main :: IO ()
main = do
    print 1
--    phs <- mapM runSleep [1,2,3,4]
--    mapM_ printResult phs

printResult :: ProcessHandle -> IO ()
printResult ph = do
    exit <- waitForProcess ph
    putStrLn $ "Process exited with " ++ show exit


runServer :: IO ProcessHandle
runServer = getInfo <$> createProcess cproc { cwd = cwd }
    where

        cproc :: CreateProcess
        cproc = proc "./HFO" args

        args :: [String]
        args = []

        getInfo :: (a, b, c, ProcessHandle) -> ProcessHandle
        getInfo = (\(_,_,_,ph) -> ph)

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO/bin"

