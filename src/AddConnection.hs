{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable #-}
module AddConnection (
      AddConn(..),
      defaultAddConn,
      addConnectionForm,
      addConnection
   ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import Data.Typeable
import Happstack.Server
import System.Log.Logger
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import Connections
import Home
import Log
import Templates
import TCPProxy
import Types
import DB

data AddConn = AddConn {
     acDesc    :: String
   , acSrcAddr :: String
   , acSrcPort :: String
   , acDstAddr :: String
   , acDstPort :: String
   , acError   :: String
   } deriving (Data, Typeable)

defaultAddConn = AddConn {
     acDesc    = ""
   , acSrcAddr = "0.0.0.0"
   , acSrcPort = ""
   , acDstAddr = ""
   , acDstPort = ""
   , acError   = ""
   }   

addConnectionForm :: AddConn -> ProxyResponse
addConnectionForm formData = do
   templateText <- liftIO $ getTemplate "addconn.html"
   let template = (newSTMP templateText :: StringTemplate String)
   ok $ toResponseBS (C.pack "text/html") (L.pack $ toString $ subst template)
   where
      subst tpl = setAttribute "addConn" formData tpl

addConnection :: ProxyResponse
addConnection = do 
      decodeBody bodyPolicy
      methodM POST
      [desc, srcAddr, srcPort, dstAddr, dstPort] <- 
         mapM look ["desc", "srcAddr", "srcPort", "dstAddr", "dstPort"]
      let formData = defaultAddConn {
           acDesc    = desc
         , acSrcAddr = srcAddr
         , acSrcPort = srcPort
         , acDstAddr = dstAddr
         , acDstPort = dstPort
         }
      msum [verify formData ,openConn formData]
   where
      verify formData = do
         if acSrcPort formData == "" || acDstAddr formData == "" || 
            acDstPort formData == ""
            then join $ liftIO $ return $ addConnectionForm formData {
                 acError = "Please fill out every field!"
               }
            else mzero
      isEmpty s = length s == 0
      openConn formData = do
         config <- ask
         let dbConn = cfgDbConn config
         let conns = cfgConns config
         join $ liftIO $ handle (\(SomeException e) ->
            return $ addConnectionForm formData { acError = show e}) $ do
            conn <- liftIO $ proxyConn $ 
               ProxyConn (acDesc formData) (acSrcAddr formData) 
                  (acSrcPort formData) (acDstAddr formData) 
                  (acDstPort formData) Nothing
            liftIO $ saveConn dbConn conns conn
            logMsg infoM $ "Proxy connection opened: " ++ show conn
            return $ home

