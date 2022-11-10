{- $Id: Types.hs,v 1.2 2012/05/18 10:29:43 moser Exp $ -}
module Types (
      ProxyConn(..),
      ProxyConfig(..),
      Config,
      ProxyResponse,
      ask
   ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Database.HDBC.Sqlite3
import Happstack.Server
import Network.Socket

data ProxyConn = ProxyConn {
     connDesc    :: String
   , connSrcAddr :: String
   , connSrcPort :: String
   , connDstAddr :: String
   , connDstPort :: String
   , connSocket  :: Maybe Socket
   } deriving (Show, Eq)

data ProxyConfig = ProxyConfig {
     cfgPort   :: Int
   , cfgDbConn :: Connection
   , cfgConns  :: TVar [ProxyConn]
   } 

type Config = ReaderT ProxyConfig
type ProxyResponse = Config (ServerPartT IO) Response

