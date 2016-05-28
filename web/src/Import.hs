{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Import where

import Yesod

pRoutes = [parseRoutes|
/ HomeR GET
/template TemplateR GET
/cadastrocliente CadastroClienteR GET
/static StaticR Static getStatic
|]