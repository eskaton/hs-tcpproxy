{- $Id: Log.hs,v 1.4 2012/05/13 15:47:25 moser Exp $ -}
module Log (
      initLog,
      logMsg,
      getLogLevel
   ) where

import Data.Char
import Control.Monad.Trans
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Logger

logName = "TCPProxy"

initLog file level = do
   h <- fileHandler file level >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "$time $loggername [$prio] $msg")
   updateGlobalLogger rootLoggerName (setLevel level . setHandlers [h])
   updateGlobalLogger logName (setLevel level . setHandlers [h])

logMsg f m = liftIO $ f logName m

getLogLevel :: String -> Maybe Priority
getLogLevel lev = 
   case map toLower lev of
      "debug"     -> Just DEBUG
      "info"      -> Just INFO
      "notice"    -> Just NOTICE
      "warning"   -> Just WARNING
      "error"     -> Just ERROR
      "critical"  -> Just CRITICAL
      "alert"     -> Just ALERT
      "emergency" -> Just EMERGENCY
      _           -> Nothing

