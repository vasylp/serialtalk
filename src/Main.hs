module Main where 

import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs, getEnvironment)
import System.IO
import Control.Monad
import Text.Regex.Posix
import Data.List
import Data.String.Utils
import ConfigParser hiding (timeout)
import qualified Data.Map as M
import Text.StringTemplate
import System.Exit

data Direction = SEND | RECV deriving Show

writeToLog :: String -> IO ()
writeToLog = putStrLn

writeToLogComm :: Direction -> String -> IO()
writeToLogComm dir str = if str == empty then return () else writeToLog $ (show dir) ++ " " ++ (strip str)

writeToLogControl :: String -> IO ()
writeToLogControl = writeToLog . strip

setSerialTimeout :: SerialPort -> Int -> IO SerialPort
setSerialTimeout sp to = do
    let settings = getSerialSettings sp
    newsp <- setSerialSettings sp settings {timeout = to}
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

dispatchInput :: SerialPort -> Int -> [(B.ByteString, (SerialPort -> B.ByteString -> IO()))] -> IO ()
dispatchInput serial count patlist = do
    runDispatch B.empty
    where 
        runDispatch bs = do
            input <- recv serial count
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

main :: IO()
main = do
    (portName:configFile:[]) <- getArgs
    rules <- parseRules configFile
    env <- getEnvironment
    
    serial <- openSerialPort portName

    sendLine serial B.empty 
    
    dispatchNext env rules (entryPoint rules) serial

    closeSerialPort serial

getRules :: Rules -> String -> Maybe [Pattern]
getRules (Rules {dispatchMap=m}) name = name `M.lookup` m

ruleToPatternDispatch :: [(String, String)] -> Rules -> Pattern -> (B.ByteString, (SerialPort -> B.ByteString -> IO()))
ruleToPatternDispatch env rules Pattern {pattern=p, textToSend=t, nextDispatcher=n} = (B.pack p, sendString env rules t n)

sendString :: [(String, String)] -> Rules -> String -> String -> SerialPort -> B.ByteString -> IO ()
sendString env rules txt disp serial _ = do
    let subst = substituteWithEnv env txt
    writeToLogControl $ "Sending line: '" ++ txt ++ "' converted to '" ++ subst ++ "'"
    sendLine serial (B.pack subst)
    dispatchNext env rules disp serial

dispatchNext :: [(String, String)] -> Rules -> String -> SerialPort -> IO()
dispatchNext env rules dn serial = do
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
            dispatchInput serial 20 ruleSet

substituteWithEnv :: [(String, String)] -> String -> String
substituteWithEnv env src = render $ setManyAttrib env $ newSTMP src
