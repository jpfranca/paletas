{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Import where

import Yesod

pRoutes = [parseRoutes|
/ HomeR GET
/teste TesteR GET
/template TemplateR GET
/static StaticR Static getStatic
|]