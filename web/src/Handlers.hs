{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where

import Import
import Foundation
import Yesod
import Yesod.Form.Bootstrap3
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)

mkYesodDispatch "WebSite" pRoutes

data ExternalLibraryName = Bootstrap | BootstrapTheme | BootstrapJs | JQuery | JQueryMask

loadExternalLibrary :: ExternalLibraryName -> Text
loadExternalLibrary Bootstrap = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
loadExternalLibrary BootstrapTheme = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
loadExternalLibrary BootstrapJs = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
loadExternalLibrary JQuery = "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
loadExternalLibrary JQueryMask = "https://igorescobar.github.io/jQuery-Mask-Plugin/js/jquery.mask.min.js"

widgetDefaultLayout = defaultLayout $ do
    setTitle "Paletas Haskell"
    toWidgetHead [hamlet|
                    <meta name=viewport content="width=device-width, initial-scale=1"> 
                    <meta charset="utf-8">
                 |]
    addStylesheetRemote $ loadExternalLibrary Bootstrap
    addStylesheetRemote $ loadExternalLibrary BootstrapTheme
    addScriptRemote $ loadExternalLibrary JQuery
    addScriptRemote $ loadExternalLibrary BootstrapJs
    addScriptRemote $ loadExternalLibrary JQueryMask
    toWidget $(whamletFile "templates/navbarDefault.hamlet")

widgetForm :: Route WebSite -> Enctype -> Widget -> Widget
widgetForm route enctype widget = [whamlet|
    <form method=post action=@{route} enctype=#{enctype}>
    ^{widget}
    <input .btn .btn-primary .btn-lg .pull-right type="submit" value="Cadastrar">
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getTemplateR :: Handler Html
getTemplateR = do 
    widgetDefaultLayout
    
