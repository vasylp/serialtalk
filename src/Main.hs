module Main where 

import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.IO
import Control.Monad
import Text.Regex.Posix
import Data.List
import ConfigParser hiding (timeout)
import qualified Data.Map as M

data Direction = SEND | RECV deriving Show

writeToLog :: String -> IO ()
writeToLog = putStrLn

writeToLogComm :: Direction -> String -> IO()
writeToLogComm dir str = if str == empty then return () else writeToLog $ (show dir) ++ " " ++ str

writeToLogControl :: String -> IO ()
writeToLogControl = writeToLog

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
    serial <- openSerialPort portName

    sendLine serial B.empty 
    
    dispatchNext rules (entryPoint rules) serial

    closeSerialPort serial

getRules :: Rules -> String -> Maybe [Pattern]
getRules (Rules {dispatchMap=m}) name = name `M.lookup` m

ruleToPatternDispatch :: Rules -> Pattern -> (B.ByteString, (SerialPort -> B.ByteString -> IO()))
ruleToPatternDispatch rules Pattern {pattern=p, textToSend=t, nextDispatcher=n} = (B.pack p, sendString rules t n)

sendString :: Rules -> String -> String -> SerialPort -> B.ByteString -> IO ()
sendString rules txt disp serial _ = do
    sendLine serial (B.pack txt)
    dispatchNext rules disp serial

dispatchNext :: Rules -> String -> SerialPort -> IO()
dispatchNext rules dn serial = do
    let dr = getRules rules dn
    case dr of
        Nothing -> do
            writeToLogControl $ "Rule '" ++ dn ++ "' not found. Finishing"
            return ()
        Just pat -> do
            writeToLogControl $ "Switching to rule '" ++ dn ++ "'"
            let ruleSet = map (ruleToPatternDispatch rules) pat
            dispatchInput serial 20 ruleSet
