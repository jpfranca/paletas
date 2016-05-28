{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Import where

import Yesod

pRoutes = [parseRoutes|
/ HomeR GET
/template TemplateR GET
/login LoginR GET POST
/logoff LogOffR GET
/admin AdminR GET
/cadastrocliente CadastroClienteR GET POST
/produto ProdutoR GET POST
/erro ErroR GET 
/static StaticR Static getStatic
|]