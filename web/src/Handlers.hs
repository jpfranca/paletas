{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where

import Import
import Yesod
import Foundation
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)

mkYesodDispatch "WebSite" pRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]