module Main where 

import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs, getEnvironment)
import System.IO
import Control.Monad
import Text.Regex.Posix
import Data.List
import Data.String.Utils
import ConfigParser
import qualified Data.Map as M
import Text.StringTemplate
import System.Exit
import Control.Concurrent
import System.IO
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Concurrent.MVar

data Direction = SEND | RECV

instance Show Direction where
    show SEND = "S"
    show RECV = "R" 

writeToLogOut :: String -> IO ()
writeToLogOut = putStrLn

writeToLogErr :: String -> IO ()
writeToLogErr = putStrLn

writeToLogComm :: Direction -> String -> IO()
writeToLogComm aDirection aString = do
    aTimestamp <- getCurrentTimeString
    if aString == empty 
        then return () 
        else writeToLogOut $ (take 13 aTimestamp) ++ " " ++ (show aDirection) ++ ": " ++ (strip aString)

getCurrentTimeString :: IO String
getCurrentTimeString = do 
    aCurrentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "%H:%M:%S.%q" aCurrentTime 

writeToLogControl :: String -> IO ()
writeToLogControl = writeToLogErr . strip

openSerialPort :: FilePath -> IO Handle
openSerialPort aFile = do
    let aSpeed = CS115200
    aHandle <- hOpenSerial aFile defaultSerialSettings {commSpeed = aSpeed}
    writeToLogControl $ "Opened serial port '" ++ aFile ++ "' speed " ++ (show aSpeed)
    return aHandle 

closeSerialPort :: Handle -> IO ()
closeSerialPort aHandle = do
    hClose aHandle 
    writeToLogControl $ "Serial port closed"

sendLine :: Handle -> String  -> IO () 
sendLine aHandle aLine = do
    writeToLogComm SEND aLine  
    hPutStr aHandle $ aLine ++ "\r"

-- Seconds to sleep before kill
timeoutGuard :: Int -> MVar () -> IO()
timeoutGuard aSeconds aVar = do
    let aMs = aSeconds * 1000000
    threadDelay aMs 
    putMVar aVar ()

exitWithError aHandle errorStatus = do
    hClose aHandle
    errorStatus

dispatchInput :: Handle -> [(String, (Handle -> String -> IO()))] -> Int -> IO ()
dispatchInput aHandle aPatterns aTimeout = do
        runDispatch []
    where
        runDispatch _ = do
            aTimeoutVar <- newEmptyMVar
            aThread <- forkIO $ timeoutGuard aTimeout aTimeoutVar
            aLine <- getLine aHandle aTimeoutVar
            killThread aThread
            writeToLogComm RECV aLine
            case find (matchString aLine) aPatterns of
                Nothing -> do
                    runDispatch []
                Just (aPattern, aCallable) -> do
                    aCallable aHandle aLine
        matchString aString (aPattern, _) = aString =~ aPattern 
        getLine aHandle aTimeoutVar = do
            isTimeout <- isEmptyMVar aTimeoutVar
            unless isTimeout $ do
                writeToLogControl "TIME IS OUT"
                exitWithError aHandle exitFailure
            isEof <- hIsEOF aHandle
            if isEof
                then do
                    -- sleep for 1 second and read again
                    threadDelay 1000000
                    getLine aHandle aTimeoutVar
                else do
                    hGetLine aHandle

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    (aDevicePath:aConfigPath:[]) <- getArgs
    aRules <- parseRules aConfigPath 
    anEnvironment <- getEnvironment
    
    aHandle <- openSerialPort aDevicePath
    hSetEcho aHandle False

    sendLine aHandle empty 
    
    dispatchNext anEnvironment aRules (entryPoint aRules) 10 aHandle 

    closeSerialPort aHandle 

getRules :: Rules -> String -> Maybe [Pattern]
getRules (Rules {dispatchMap = aDispatchMap}) aDispatcherName = aDispatcherName `M.lookup` aDispatchMap

ruleToPatternDispatch :: [(String, String)] -> Rules -> Pattern -> (String, (Handle -> String -> IO()))
ruleToPatternDispatch anEnv rules Pattern {pattern=aPattern, textToSend=aTextToSend, nextDispatcher=aNextDispatcher, ConfigParser.timeout=aTimeout} = 
    (aPattern, sendString anEnv rules aTextToSend aNextDispatcher aTimeout)

sendString :: [(String, String)] -> Rules -> String -> String -> Int -> Handle -> String -> IO ()
sendString anEnvironment aRules aTextToSend aDispatcher aTimeout aHandle _ = do
    let aSubstString = substituteWithEnv anEnvironment aTextToSend 
    unless (null aSubstString) $ do
        writeToLogControl $ "Sending line: '" ++ aTextToSend ++ "' converted to '" ++ aSubstString ++ "'"
        sendLine aHandle aSubstString 
    dispatchNext anEnvironment aRules aDispatcher aTimeout aHandle 

dispatchNext :: [(String, String)] -> Rules -> String -> Int -> Handle -> IO()
dispatchNext anEnvironment aRules aDispatcherName aTimeout aHandle = do
    let aDispatcherRule = getRules aRules aDispatcherName
    case aDispatcherRule of
        Nothing -> do
            case aDispatcherName of
                "exitFailure" -> do
                     writeToLogControl $ "Rule '" ++ aDispatcherName ++ "' Exiting with failure code"
                     exitWithError aHandle exitFailure
                "exitSuccess" -> do
                     writeToLogControl $ "Rule '" ++ aDispatcherName ++ "' Exiting with success code"
                     exitWithError aHandle exitSuccess
                _ -> do 
                    writeToLogControl $ "Rule '" ++ aDispatcherName ++ "' not found. Finishing with success"
                    return ()
        Just aPattern -> do
            writeToLogControl $ "Switching to rule '" ++ aDispatcherName ++ "'"
            let ruleSet = map (ruleToPatternDispatch anEnvironment aRules) aPattern
            dispatchInput aHandle ruleSet aTimeout 

substituteWithEnv :: [(String, String)] -> String -> String
substituteWithEnv anEnvironment aSourceString = render $ setManyAttrib anEnvironment $ newSTMP aSourceString 
