module Main where

import System.IO       (hGetContents, hSetBuffering, BufferMode(..), Handle(..))
import System.Process  
import Control.Monad
import HFO.Server
-- import System.TimeIt

main :: IO ()
main = do
    let serverConf = defaultServer { offenseAgents = 1 }
    (out, ph) <- runServer serverConf
    -- hGetContents out >>= putStrLn
    printResult ph  -- | TODO this does not work. Somehow the HFO binary does not return an exit code?...




printResult :: ProcessHandle -> IO ()
printResult ph = do
    exit <- waitForProcess ph
    putStrLn $ "Process exited with " ++ show exit


runServer :: ServerConf -> IO (Handle, ProcessHandle)
runServer conf = getInfo <$> createProcess cproc { cwd = cwd }
    where

        cproc :: CreateProcess
        cproc = proc "./bin/HFO" args

        args :: [String]
        args = toFlags conf

        getInfo :: (a, Maybe Handle, c, ProcessHandle) -> (Handle, ProcessHandle)
        getInfo = (\(_,Just out,_,ph) -> (out, ph))

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO"

