{-# LANGUAGE FlexibleContexts #-}

module DB (
   dbOpen,
   dbClose,
   dbRun,
   Connection (..),
   commit
) where

import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3

dbName :: FilePath -> FilePath
dbName = flip (++) ".db"

dbExists :: FilePath -> IO Bool
dbExists = doesFileExist . dbName

dbCreate :: FilePath -> IO Connection
dbCreate db = do
   conn <- connectSqlite3 $ dbName db
   run conn "CREATE TABLE connection (\
               \ conDesc    VARCHAR NOT NULL, \
               \ conSrcAddr VARCHAR NOT NULL, \
               \ conSrcPort VARCHAR NOT NULL, \
               \ conDstaddr VARCHAR NOT NULL, \
               \ conDstPort VARCHAR NOT NULL  \
            \)" []
   return conn

dbOpen :: String -> IO Connection
dbOpen db = do
   dbFound <- dbExists db
   if dbFound
      then connectSqlite3 $ dbName db
      else do
         conn <- dbCreate db
         commit conn
         return conn
   
dbClose conn = disconnect conn

dbRun conn stmt vals = run conn stmt $ map toSql vals
