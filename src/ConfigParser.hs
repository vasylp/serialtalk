{-# LANGUAGE DeriveDataTypeable #-}
module ConfigParser where

import Text.JSON
import Text.JSON.Generic
import qualified Data.Map as M

data Pattern = Pattern {
    pattern :: String,
    textToSend :: String,
    nextDispatcher :: String,
    timeout :: Int
} deriving (Eq, Show, Data, Typeable)

data Dispatcher = Dispatcher {
    name :: String,
    dispatch :: [Pattern]
} deriving (Eq, Show, Data, Typeable)

data Config = Config {
    mainDispatcher :: String,
    dispatchers :: [Dispatcher]
} deriving (Eq, Show, Data, Typeable)

data Rules = Rules {
    entryPoint :: String,
    dispatchMap :: M.Map String [Pattern]
} deriving (Eq, Show)

parseRules :: String -> IO Rules
parseRules fileName = do
    file <- readFile fileName
    let jsonConfig = decodeJSON file :: Config
    return $ Rules {entryPoint = (mainDispatcher jsonConfig),
                    dispatchMap = createMap (dispatchers jsonConfig)}
    where 
        createMap :: [Dispatcher] -> M.Map String [Pattern]
        createMap = M.fromList . map parseDispatcher
        parseDispatcher :: Dispatcher -> (String, [Pattern])
        parseDispatcher d = (name d, dispatch d)

