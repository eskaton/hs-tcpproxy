{- $Id: HomePage.hs,v 1.4 2012/05/18 10:29:43 moser Exp $ -}
{-# LANGUAGE OverloadedStrings #-}
module Home (
      home
   ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.String
import Text.StringTemplate

import TCPProxy
import Templates
import Types

home :: ProxyResponse
home = do
   config <- ask
   let conns = cfgConns config
   templateText <- liftIO $ getTemplate "/index.html" 
   let template = (newSTMP templateText :: StringTemplate String)
   connsText <- liftIO $ readTVarIO conns
   ok $ toResponseBS (C.pack "text/html") (L.pack $ toString $ 
      setAttribute "connections" (getConnTable connsText) template)

getConnTable :: [ProxyConn] -> String
getConnTable [] = (++) "No connections defined" $ renderHtml $ do  
   H.br 
   H.br
getConnTable conns = renderHtml $ renderTable $ forM_ conns renderConn
   where renderTable :: H.Html -> H.Html
         renderTable body =
            H.table ! A.class_ "c-data-grid" $ do
               H.thead $ H.tr $ do
                  H.td $ "Description"
                  H.td $ "Source Address"
                  H.td $ "Source Port"
                  H.td $ "Destination Address"
                  H.td $ "Destination Port"
                  H.td $ "Action"
               H.tbody $ body
         renderConn :: ProxyConn -> H.Html
         renderConn conn@(ProxyConn d sa sp da dp _) = 
            H.tr $ do
               H.td $ H.toHtml d
               H.td $ H.toHtml sa
               H.td $ H.toHtml sp
               H.td $ H.toHtml da
               H.td $ H.toHtml dp
               H.td $ do
                  "["
                  H.a ! A.href (H.toValue $ action conn) $ do
                     H.img ! 
                        A.src "images/delete.png" ! 
                        A.style "width: 16px; vertical-align: text-top;"
                     H.preEscapedString "&nbsp;"
                     "close"
                  "]"
         action :: ProxyConn -> String
         action conn = "/closeconn?connId=" ++ connectionId conn
