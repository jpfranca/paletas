{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}

module Foundation where

import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    

data WebSite = WebSite { getStatic :: Static, connPool :: ConnectionPool } 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Produto json
    nome Text
    valor Double
    deriving Show

Cliente json
    razaoSocial Text
    nomeFantasia Text
    cnpj Text
    inscricaoEstadual Text
    telefone Text
    email Text
    senha Text
    deriving Show

Pedido json
    clienteId ClienteId
    dataSolicitacao Text
    prazoEstimado Int
    valorTotal Double
    deriving Show
    
PedidoProduto json
    pedidoId PedidoId
    produtoId ProdutoId
    quantidade Int
    valorTotal Double
    deriving Show
|]

staticFiles "static"

mkYesodData "WebSite" pRoutes

instance YesodPersist WebSite where
   type YesodPersistBackend WebSite = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod WebSite where
    authRoute _ = Just LoginR -- rota default para acesso não autorizado
    isAuthorized HomeR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized CadastroClienteR _ = return Authorized
    isAuthorized AdminR _ = isUserAdmin
    isAuthorized ClienteR _ = isUserAdmin
    isAuthorized (ClienteIdR _) _ = isUserAdmin
    isAuthorized ProdutoR _= isUserAdmin
    isAuthorized (ProdutoIdR _) _ = isUserAdmin
    isAuthorized _ _ = isLogged

isLogged = do
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isUserAdmin = do
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Acesso não autorizado"
        Nothing -> Unauthorized "Acesso não autorizado"

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage WebSite FormMessage where
    renderMessage _ _ = defaultFormMessage