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
writeToLogComm dir str = do
    tstr <- getCurrentTimeString
    if str == empty then return () else writeToLogOut $ tstr ++ " " ++ (show dir) ++ ":" ++ (strip str)

getCurrentTimeString :: IO String
getCurrentTimeString = do currTime <- getCurrentTime
                          return $ formatTime defaultTimeLocale "%H:%M:%S.%q" currTime

writeToLogControl :: String -> IO ()
writeToLogControl = writeToLogErr . strip

setSerialTimeout :: SerialPort -> Int -> IO SerialPort
setSerialTimeout sp to = do
    let settings = getSerialSettings sp
    newsp <- setSerialSettings sp settings {System.Hardware.Serialport.timeout = to}
    writeToLogControl $ "Reset the timeout to " ++ (show to)
    return newsp

openSerialPort :: FilePath -> IO SerialPort
openSerialPort fp = do
    let speed = CS115200
    serial <- openSerial fp defaultSerialSettings {commSpeed = speed}
    writeToLogControl $ "Opened serial port '" ++ fp ++ "' speed " ++ (show speed)
    return serial

closeSerialPort :: SerialPort -> IO ()
closeSerialPort s = do
    closeSerial s
    writeToLogControl $ "Serial port closed"

sendLine :: SerialPort -> B.ByteString -> IO Bool
sendLine sp l = do
    writeToLogComm SEND $ B.unpack l
    let str = B.snoc l '\r'
    bytes <- send sp str 
    return (bytes == B.length str)

-- Seconds to sleep before kill
timeoutGuard :: Int -> MVar () -> IO()
timeoutGuard secs mv = do
    let microSeconds = secs * 1000000
    threadDelay microSeconds
    putMVar mv ()

dispatchInput :: SerialPort -> Int -> [(B.ByteString, (SerialPort -> B.ByteString -> IO()))] -> Int -> IO ()
dispatchInput serial count patlist tout = do
    runDispatch B.empty
    where 
        runDispatch bs = do
	    mv <- newEmptyMVar
            thId <- forkIO $ timeoutGuard tout mv
            input <- receivechars mv serial count
            killThread thId
            let str = bs `B.append` input
            case find (predicate str) patlist of
                Nothing -> do
                    let (prevLines, lastLine) = B.spanEnd (/= '\n') str
                    writeToLogComm RECV $ B.unpack prevLines
                    runDispatch lastLine
                Just (pat, cal) -> do
                    writeToLogComm RECV $ B.unpack str 
                    cal serial str
        predicate s (pat, _) = s =~ pat
	receivechars mv s cnt = do
		x <- recv s cnt
		v <- isEmptyMVar mv
		if not v then do
			writeToLogControl "TIME IS OUT"
			exitFailure
				    else do
			return ()
		if B.null x then receivechars mv s cnt else return x

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    (portName:configFile:[]) <- getArgs
    rules <- parseRules configFile
    env <- getEnvironment
    
    serial <- openSerialPort portName

    sendLine serial B.empty 
    
    dispatchNext env rules (entryPoint rules) 10 serial 

    closeSerialPort serial

getRules :: Rules -> String -> Maybe [Pattern]
getRules (Rules {dispatchMap=m}) name = name `M.lookup` m

ruleToPatternDispatch :: [(String, String)] -> Rules -> Pattern -> (B.ByteString, (SerialPort -> B.ByteString -> IO()))
ruleToPatternDispatch env rules Pattern {pattern=p, textToSend=t, nextDispatcher=n, ConfigParser.timeout=to} = (B.pack p, sendString env rules t n to)

sendString :: [(String, String)] -> Rules -> String -> String -> Int -> SerialPort -> B.ByteString -> IO ()
sendString env rules txt disp tout serial _ = do
    let subst = substituteWithEnv env txt
    writeToLogControl $ "Sending line: '" ++ txt ++ "' converted to '" ++ subst ++ "'"
    sendLine serial (B.pack subst)
    dispatchNext env rules disp tout serial

dispatchNext :: [(String, String)] -> Rules -> String -> Int -> SerialPort -> IO()
dispatchNext env rules dn tout serial = do
    let dr = getRules rules dn
    case dr of
        Nothing -> do
            case dn of
                "exitFailure" -> do
                     writeToLogControl $ "Rule '" ++ dn ++ "' Exiting with failure code"
                     exitFailure
                "exitSuccess" -> do
                     writeToLogControl $ "Rule '" ++ dn ++ "' Exiting with success code"
                     exitSuccess
                _ -> do 
                    writeToLogControl $ "Rule '" ++ dn ++ "' not found. Finishing with success"
                    return ()
        Just pat -> do
            writeToLogControl $ "Switching to rule '" ++ dn ++ "'"
            let ruleSet = map (ruleToPatternDispatch env rules) pat
            dispatchInput serial 20 ruleSet tout

substituteWithEnv :: [(String, String)] -> String -> String
substituteWithEnv env src = render $ setManyAttrib env $ newSTMP src
