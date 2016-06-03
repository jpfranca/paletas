{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, DeriveGeneric #-}

module Foundation where

import Import
import Yesod
import Yesod.Static
import Data.Text
import Data.Time
import Data.Int
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Database.Persist.Postgresql
import Control.Monad
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    

data WebSite = WebSite{getStatic :: Static, connPool :: ConnectionPool } 

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
    email Text Unique
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
    deriving Show
|]

data PedidoProdutoAux = PedidoProdutoAux {pedidoProduto :: PedidoProduto, produto :: Produto} deriving (Show, Generic)

data PedidoAux = PedidoAux {pedidoId :: PedidoId, pedido :: Pedido, cliente :: Cliente} deriving (Show, Generic)

instance ToJSON PedidoProdutoAux where
 toJSON (PedidoProdutoAux pedidoProduto produto) = object [ "pedidoProduto" .= (toJSON pedidoProduto),  "produto" .= (toJSON produto) ]
 
instance ToJSON PedidoAux where
 toJSON (PedidoAux pedidoId pedido cliente) = object [ "pedidoId" .= (show $ fromSqlKey pedidoId), "pedido" .= (toJSON pedido),  "cliente" .= (toJSON cliente) ]

parseToPedidoProdutoAux :: PedidoProduto -> Produto -> PedidoProdutoAux
parseToPedidoProdutoAux pedidoProduto produto = (PedidoProdutoAux pedidoProduto produto)

parseToPedidoAux :: PedidoId -> Pedido -> Cliente -> PedidoAux
parseToPedidoAux pedidoId pedido cliente = (PedidoAux pedidoId pedido cliente)


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
    isAuthorized LogOffR _ = isLogged
    isAuthorized CadastroClienteR _ = onlyLoggoff
    isAuthorized AdminR _ = onlyAdmin
    isAuthorized ProdutoR _ = onlyAdmin
    isAuthorized (ProdutoIdR _) _ = onlyAdmin
    isAuthorized ClienteR _ = onlyAdmin
    isAuthorized (ClienteIdR _) _ = onlyAdmin
    isAuthorized PerfilR _ = onlyClienteUser
    isAuthorized (PerfilAlteraDadosR _) _ = onlyClienteUser
    isAuthorized (PerfilAlteraSenha _) _ = onlyClienteUser
    isAuthorized PedidoR _ = onlyClienteUser
    isAuthorized PedidoProdutoR _ = onlyClienteUser
    isAuthorized PedidoSolicitacaoR _ = onlyClienteUser
    isAuthorized _ _ = return Authorized

isLogged = do
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Just _ -> Authorized
        _ -> AuthenticationRequired
        
onlyLoggoff = do
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Just _ -> Unauthorized "Usuários do sistema não tem permissao para visualizar estas informações"
        _ -> Authorized

onlyAdmin = do 
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Just "admin" -> Authorized
        _ -> AuthenticationRequired

onlyClienteUser = do
    sessionUserId <- lookupSession "_ID"
    return $ case sessionUserId of
        Just "admin" -> Unauthorized "Usuarios administrativos não tem permissão para visualizar essas informações"
        Just _ -> Authorized
        Nothing -> AuthenticationRequired

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage WebSite FormMessage where
    renderMessage _ _ = defaultFormMessage