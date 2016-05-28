{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Import where

import Yesod

pRoutes = [parseRoutes|
/ HomeR GET
/template TemplateR GET
/login LoginR GET
/cadastrocliente CadastroClienteR GET POST
/erro ErroR GET 
/static StaticR Static getStatic
|]