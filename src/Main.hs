{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable #-}
module Main (
   main
) where 

import qualified Data.List.Utils as LU
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Data.List
import Data.Maybe
import Data.Text.Lazy (unpack)
import Database.HDBC.Sqlite3
import Happstack.Server
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Log.Logger

import Connections
import Log
import AddConnection
import Home
import TCPProxy
import Types
import DB

data Options = Options { 
     optPort :: String,
     optLogLevel :: String,
     optLogFile :: String
   } deriving Show

defaultOptions :: Options
defaultOptions = Options { 
      optPort = "8000", 
      optLogLevel = "info",
      optLogFile = "tcpproxy.log"
   }

options :: [OptDescr (Options -> IO Options)]
options =
   [ Option ['h'] ["help"]
      (NoArg (\_ -> do
                hPutStrLn stderr usage
                exitWith ExitSuccess))
      "Show help"
   , Option ['p'] ["port"]
      (ReqArg (\arg opt -> return opt { optPort = arg }) "PORT")
      "Port number"
   , Option ['l'] ["log-level"]
      (ReqArg (\arg opt -> return opt { optLogLevel = arg })
         "LEVEL")
      "Log level"
   , Option ['f'] ["log-file"]
      (ReqArg (\arg opt -> return opt { optLogFile = arg })
         "FILE")
      "Log file"
   ]

usage = usageInfo header options
   where
      header = "Usage: TCPProxy [OPTION...]"

main :: IO ()
main = do 
   args <- getArgs
   let (actions, nonOptions, errors) = getOpt RequireOrder options args
   opts <- foldl (>>=) (return defaultOptions) actions
   case getLogLevel $ optLogLevel opts of
      Just lev -> initLog (optLogFile opts) lev
      Nothing ->
         ioError $ userError $ "Invalid log level: " ++ 
            (show $ optLogLevel opts)
   conns <- atomically $ newTVar []
   dbConn <- dbOpen "tcpproxy"
   let config = ProxyConfig {
        cfgPort = read $ optPort opts
      , cfgDbConn = dbConn
      , cfgConns = conns
      }
   loadConns dbConn >>= startConns conns 
   simpleHTTP (conf config) $ basicAuth "TCPProxy" authMap $ 
      runReaderT tcpProxy config
   where
      authMap = M.fromList [("admin", "admin")]
      conf config = nullConf { port = cfgPort config}
      startConns conns pConns = mapM_ (startConn conns) pConns
      startConn conns pConn = do 
         newPConn <- proxyConn pConn
         atomically $ readTVar conns >>= writeTVar conns . (newPConn :)
   
tcpProxy :: ProxyResponse
tcpProxy = do 
      config <- ask
      let dbConn = cfgDbConn config
      let conns = cfgConns config
      msum [ dir "css" $ serveDir "resources/css"
           , dir "images" $ serveDir "resources/images"
           , dir "addconn" $ addConnectionForm defaultAddConn
           , dir "addConnPost" $ addConnection 
           , dir "closeconn" $ deleteConnection 
           , home
           ]

serveDir :: String -> ProxyResponse
serveDir dir = do
   serveDirectory DisableBrowsing [] dir

deleteConnection :: ProxyResponse
deleteConnection = do 
      config <- ask
      connId <- liftM unpack $ lookText "connId"
      let conns = cfgConns config
      let dbConn = cfgDbConn config
      liftIO $ do 
         conn <- findConn conns connId
         maybe (return ()) (doClose dbConn conns) conn
      home
   where 
      doClose dbConn conns conn = do
         closeConn conn
         deleteConn dbConn conns conn
         logMsg infoM $ "Proxy connection closed: " ++ showConn conn
      showConn conn = connSrcAddr conn ++ ":" ++ connSrcPort conn ++ "->" ++ 
         connDstAddr conn ++ ":" ++ connDstPort conn
