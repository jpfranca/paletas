{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)


data WebSite = WebSite{connPool :: ConnectionPool}

instance Yesod WebSite

instance YesodPersist WebSite where
   type YesodPersistBackend WebSite = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

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
    deriving Show

Usuario json
    email Text
    clienteId ClienteId
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

mkYesod "WebSite" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

connStr = "dbname=d649v6hs211uh host=ec2-54-204-46-221.compute-1.amazonaws.com user=ywphdgpebmhtlx password=7HZV3l99vZecCXAL7yQLAjx7WI port=5432"
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (WebSite pool)