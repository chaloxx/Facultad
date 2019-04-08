
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)


main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock


sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        ("echo") -> echoCommand handle cmd
        ("add") -> addCommand handle cmd
        _ -> do hPutStrLn handle "Unknown command"
    commandProcessor handle
    
    
    
echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = putStrLn (foldr (++) [] (tail cmd))
    
    
addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = putStrLn (show ((read $ cmd !! 1) + (read $ cmd !! 2)))



