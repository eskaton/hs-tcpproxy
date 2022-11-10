{- $Id: TCPProxy.hs,v 1.7 2012/05/18 10:29:43 moser Exp $ -}
{-# LANGUAGE FlexibleInstances #-}
module TCPProxy (
   proxyConn,
   closeConn,
   connectionId
) where

import Network.Socket
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Fix (fix)
import qualified Data.ByteString as B
import System.Log.Logger
import Data.Maybe 

import Log
import Types

closeConn :: ProxyConn -> IO ()
closeConn conn = do
   maybe (return ()) close $ connSocket conn

proxyConn :: ProxyConn -> IO ProxyConn
proxyConn pConn = do
   let [srcAddr, srcPort, dstAddr, dstPort] = map (\f -> f pConn) 
         [connSrcAddr, connSrcPort, connDstAddr, connDstPort]
   sock <- socket AF_INET Stream 0
   setSocketOption sock ReuseAddr 1
   locAddr <- lookupHost (Just srcAddr) (Just srcPort)
   bind sock locAddr
   listen sock 8 
   forkIO $ fix $ \loop -> do
      (src, peer) <- accept sock
      logMsg infoM $ "Connection from " ++ show peer
      srcHdl <- getHandle src
      handle (\(SomeException e) -> 
         (putStrLn $ show e) >> hClose srcHdl) $ do
            dst <- getConn dstAddr dstPort
            dstHdl <- getHandle dst
            runConn srcHdl dstHdl
            loop 
   return $ pConn { connSocket = Just sock }

connectionId :: ProxyConn -> String
connectionId conn@(ProxyConn _ sa sp da dp _) = sa ++ "_" ++ sp ++ "_" ++ da ++ "_" ++ dp

getHandle :: Socket -> IO Handle
getHandle sock = do
   hdl <- socketToHandle sock ReadWriteMode 
   hSetBinaryMode hdl True
   return hdl

runConn :: Handle -> Handle -> IO ()
runConn src dst = do
   logMsg debugM $ "Established connection: " 
      ++ (show src) ++ " -> " ++ (show dst)
   forkIO $ write src dst
   forkIO $ write dst src
   return ()

getConn :: String -> String -> IO Socket
getConn host port = do
   addr <- lookupHost (Just host) (Just port)
   sock <- socket AF_INET Stream 0 
   connect sock addr
   return sock

inetV4Hints = defaultHints {addrFamily = AF_INET}

lookupHost :: Maybe String -> Maybe String -> IO SockAddr
lookupHost host port = do
   logMsg debugM $ "Looking up " ++ (maybeHost host) ++ ":" ++ (maybePort port)
   addrInfo <- getAddrInfo (Just inetV4Hints) host port
   return $ addrAddress $ head addrInfo

write :: Handle -> Handle -> IO ()
write src dst = do 
   handle (\(SomeException _) -> hClose src >> hClose dst) $ 
      fix $ \loop -> do
         B.hGetSome src 4096 >>= B.hPut dst >> hFlush dst >> loop

maybePort :: Maybe String -> String
maybePort port = maybe "0" id port

maybeHost :: Maybe String -> String
maybeHost host = maybe "0.0.0.0" id host
