{- $Id: Connections.hs,v 1.3 2012/05/18 10:29:43 moser Exp $ -}
module Connections (
      deleteConn,
      findConn,
      saveConn,
      loadConns
   ) where

import Control.Concurrent.STM
import Data.Maybe (listToMaybe, fromJust)
import Database.HDBC

import DB
import TCPProxy
import Types

saveConn :: Connection -> TVar [ProxyConn] -> ProxyConn -> IO ()
saveConn dbConn conns conn = do
   atomically $ readTVar conns >>= writeTVar conns . (conn :)
   insertConn dbConn conn
   where
      insertConn dbConn conn = do
         dbRun dbConn "INSERT INTO connection VALUES (?, ?, ?, ?, ?)"
            [connDesc conn, connSrcAddr conn, connSrcPort conn, 
               connDstAddr conn, connDstPort conn]
         commit dbConn

findConn :: TVar [ProxyConn] -> String -> IO (Maybe ProxyConn)
findConn conns connId = do
   var <- atomically $ readTVar conns
   return $ listToMaybe $ filter (\c -> connectionId c == connId) var

deleteConn :: Connection -> TVar [ProxyConn] -> ProxyConn -> IO ()
deleteConn dbConn conns conn = do
   atomically $ readTVar conns >>= writeTVar conns . removeConn conn
   deleteConn dbConn conn
   where 
      removeConn :: ProxyConn -> [ProxyConn] -> [ProxyConn]
      removeConn conn = filter $ (/=) conn
      deleteConn dbConn conn = do
         dbRun dbConn "DELETE FROM connection \
                      \ WHERE conSrcAddr = ? \
                      \   AND conSrcPort = ? \
                      \   AND conDstAddr = ? \
                      \   AND conDstPort = ?"
            [connSrcAddr conn, connSrcPort conn, connDstAddr conn, connDstPort conn]
         commit dbConn

loadConns :: Connection -> IO [ProxyConn]
loadConns dbConn = do
   rows <- quickQuery' dbConn "SELECT conDesc, conSrcAddr, conSrcPort, \
                               \      conDstAddr, conDstPort \
                               \ FROM connection" []
   mapM rowToStruct rows
   where
      rowToStruct :: [SqlValue] -> IO ProxyConn
      rowToStruct row = do
         let strRow = map (fromJust . fromSql) row
         return ProxyConn {
              connDesc    = strRow !! 0
            , connSrcAddr = strRow !! 1
            , connSrcPort = strRow !! 2
            , connDstAddr = strRow !! 3
            , connDstPort = strRow !! 4
            , connSocket = Nothing
            }
