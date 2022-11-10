{- $Id: Templates.hs,v 1.1 2012/05/12 17:14:08 moser Exp $ -}
module Templates (
      bodyPolicy,
      getTemplate
   ) where

import Happstack.Server

bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1024 1024)

getTemplate = readFile . (++) "resources/tpl/"
