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
import Text.Julius
import Control.Monad.Logger (runStdoutLoggingT)

mkYesodDispatch "WebSite" pRoutes

data ExternalLibraryName = Bootstrap | BootstrapTheme | BootstrapJs | JQuery | JQueryMask

loadExternalLibrary :: ExternalLibraryName -> Text
loadExternalLibrary Bootstrap = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
loadExternalLibrary BootstrapTheme = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
loadExternalLibrary BootstrapJs = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
loadExternalLibrary JQuery = "https://code.jquery.com/jquery-1.12.4.min.js"
loadExternalLibrary JQueryMask = "https://igorescobar.github.io/jQuery-Mask-Plugin/js/jquery.mask.min.js"

getNavBar :: Maybe Text -> Widget
getNavBar (Just "admin") = $(whamletFile "templates/navbarAdmin.hamlet")
getNavBar (Just _) = $(whamletFile "templates/navbarUser.hamlet")
getNavBar Nothing = $(whamletFile "templates/navbarDefault.hamlet")

widgetDefaultLayout :: Widget -> HandlerT WebSite IO Html
widgetDefaultLayout widget = defaultLayout $ do
    sessionUserId <- lookupSession "_ID"
    setTitle "Paletas Haskell"
    toWidgetHead [hamlet|
        <meta name=viewport content="width=device-width, initial-scale=1"> 
        <meta charset="utf-8">
    |]
    addStylesheetRemote $ loadExternalLibrary Bootstrap
    addStylesheetRemote $ loadExternalLibrary BootstrapTheme
    toWidgetHead [lucius|
        table thead tr td {
            font-weight: bold;
        }
        
        table thead tr th {
            font-weight: bold;
        }
    |]
    addScriptRemote $ loadExternalLibrary JQuery
    addScriptRemote $ loadExternalLibrary BootstrapJs
    addScriptRemote $ loadExternalLibrary JQueryMask
    getNavBar sessionUserId
    toWidget [whamlet|
        <div .container> 
            ^{widget}
    |] 
    
formLogin :: Form (Text,Text)
formLogin = renderBootstrap3 BootstrapBasicForm $ (,) <$>
                areq emailField (bfs ("Email" :: Text)) Nothing <*>
                areq passwordField (bfs ("Senha" :: Text)) Nothing

formCadastroCliente :: Form Cliente
formCadastroCliente = renderBootstrap3 BootstrapBasicForm $ Cliente <$>
                        areq textField (bfs ("Razão social" :: Text)) Nothing <*>
                        areq textField (bfs ("Nome fantasia" :: Text)) Nothing <*>
                        areq textField (bfs ("CNPJ" :: Text)) Nothing <*>
                        areq textField (bfs ("Inscrição Estadual" :: Text)) Nothing <*>
                        areq textField (bfs ("Telefone" :: Text)) Nothing <*>
                        areq emailField (bfs ("Email" :: Text)) Nothing <*>
                        areq passwordField (bfs ("Senha" :: Text)) Nothing
                        
formAlteraCliente :: Cliente -> Form (Text, Text, Text, Text)
formAlteraCliente cliente = renderBootstrap3 BootstrapBasicForm $ (,,,) <$>
                        areq textField (bfs ("Razão social" :: Text)) (Just (clienteRazaoSocial cliente)) <*>
                        areq textField (bfs ("Nome fantasia" :: Text)) (Just (clienteNomeFantasia cliente)) <*>
                        areq textField (bfs ("Telefone" :: Text)) (Just (clienteTelefone cliente)) <*>
                        areq emailField (bfs ("Email" :: Text)) (Just (clienteEmail cliente))

formAlteraSenha :: Form (Text, Text)
formAlteraSenha = renderBootstrap3 BootstrapBasicForm $ (,) <$>
                    areq passwordField (bfs ("Senha atual" :: Text)) Nothing <*>
                    areq passwordField (bfs ("Nova senha" :: Text)) Nothing

formCadastroProduto :: Form Produto
formCadastroProduto = renderBootstrap3 BootstrapBasicForm $ Produto <$>
                        areq textField (bfs ("Nome" :: Text)) Nothing <*>
                        areq doubleField (bfs ("Valor" :: Text)) Nothing

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getErroR :: Handler Html
getErroR = widgetDefaultLayout $ do
    toWidgetHead [lucius|
        h3 {
            color: red;
        }
    |]
    toWidget [whamlet|
        <h3> Ocorreu um erro inesperado no sistema
        
        <a href=@{HomeR}>Voltar a pagina inicial
    |]

getTemplateR :: Handler Html
getTemplateR = do 
    widgetDefaultLayout [whamlet|Hello World|]
    
getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost formLogin
    widgetDefaultLayout $ do
    toWidget $ $(juliusFile "templates/login.julius")
    $(whamletFile "templates/login.hamlet")
    
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin@paletas.com","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (email,senha) -> do 
                   clienteEntity <- runDB $ selectFirst [ClienteEmail ==. email, ClienteSenha ==. senha] []
                   case clienteEntity of
                        Nothing -> redirect LoginR
                        Just (Entity clienteId cliente) -> do 
                            setSession "_ID" (pack $ show $ fromSqlKey clienteId) 
                            setSession "_EMAIL" (clienteEmail cliente)
                            setSession "_SENHA" (clienteSenha cliente) 
                            >> redirect PerfilR

getLogOffR :: Handler Html
getLogOffR = do
    deleteSession "_ID"
    deleteSession "_EMAIL"
    deleteSession "_SENHA"
    redirect LoginR

getAdminR :: Handler Html
getAdminR = do
    widgetDefaultLayout $(whamletFile "templates/admin.hamlet")

getCadastroClienteR :: Handler Html
getCadastroClienteR = do
    (widget, enctype) <- generateFormPost formCadastroCliente
    widgetDefaultLayout $ do
    toWidget $ $(juliusFile "templates/cadastroCliente.julius")
    $(whamletFile "templates/cadastroCliente.hamlet")

postCadastroClienteR :: Handler Html
postCadastroClienteR = do
           ((result, _), _) <- runFormPost formCadastroCliente
           case result of 
               FormSuccess usuario -> (runDB $ insert usuario) >>= \usuarioId -> redirect CadastroClienteR
               _ -> redirect ErroR
               
getProdutoR :: Handler Html
getProdutoR = do
    (widget, enctype) <- generateFormPost formCadastroProduto
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    widgetDefaultLayout $ do
    toWidget $ $(juliusFile "templates/produto.julius")
    $(whamletFile "templates/produto.hamlet")
    
postProdutoR :: Handler Html
postProdutoR = do
    ((result, _), _) <- runFormPost formCadastroProduto
    case result of 
        FormSuccess produto -> (runDB $ insert produto) >> redirect ProdutoR
        _ -> redirect ErroR
        
postProdutoIdR :: ProdutoId -> Handler Html
postProdutoIdR produtoId = do
     runDB $ delete produtoId
     redirect ProdutoR
     
getClienteR :: Handler Html
getClienteR = do
    clientes <- runDB $ selectList [] [Asc ClienteNomeFantasia]
    widgetDefaultLayout $ do
    toWidget $ $(juliusFile "templates/cliente.julius")
    $(whamletFile "templates/cliente.hamlet")
    
getClienteIdR :: ClienteId -> Handler ()
getClienteIdR clienteId = do
    cliente <- runDB $ get404 clienteId
    sendResponse $ toJSON cliente

getPerfilR :: Handler Html
getPerfilR  = do
    sessionUserId <- lookupSession "_ID"
    getPerfilPage sessionUserId

getPerfilPage :: Maybe Text -> Handler Html
getPerfilPage Nothing = redirect LoginR
getPerfilPage (Just "admin") = redirect AdminR
getPerfilPage (Just _) = do
    maybeEmail <- lookupSession "_EMAIL"
    maybeSenha <- lookupSession "_SENHA"
    paginaPerfil maybeEmail maybeSenha

paginaPerfil :: Maybe Text -> Maybe Text -> Handler Html
paginaPerfil (Nothing) (Nothing) = redirect ErroR
paginaPerfil (Just email) (Just senha) = do
    clienteEntity <- runDB $ selectFirst [ClienteEmail ==. email, ClienteSenha ==. senha] []
    case clienteEntity of
        Nothing -> redirect ErroR
        Just (Entity clienteId cliente) -> do
            (widgetDados, enctypeDados) <- generateFormPost (formAlteraCliente cliente)
            (widgetSenha, enctypeSenha) <- generateFormPost formAlteraSenha
            widgetDefaultLayout $ do
            toWidget $ $(juliusFile "templates/perfil.julius")
            $(whamletFile "templates/perfil.hamlet")
    
postPerfilAlteraDadosR :: ClienteId -> Handler Html
postPerfilAlteraDadosR clienteId = do
    ((result, _), _) <- runFormPost (formAlteraCliente (Cliente "" "" "" "" "" "" ""))
    case result of 
        FormSuccess (razaoSocial, nomeFantasia, telefone, email) -> do
            runDB $ update clienteId [ClienteRazaoSocial =. razaoSocial, 
                                      ClienteNomeFantasia =. nomeFantasia,
                                      ClienteTelefone =. telefone,
                                      ClienteEmail =. email]
            setSession "_EMAIL" email
            redirect PerfilR
        _ -> redirect ErroR
        
postPerfilAlteraSenha :: ClienteId -> Handler Html
postPerfilAlteraSenha clienteId = do
    ((result, _), _) <- runFormPost formAlteraSenha
    case result of
        FormSuccess (senhaAtual, novaSenha) -> do
            clienteEntity <- runDB $ selectFirst [ClienteId ==. clienteId, ClienteSenha ==. senhaAtual] []
            case clienteEntity of
                Nothing -> redirect ErroR
                Just (Entity cliId cliente) -> do
                    runDB $ update cliId [ClienteSenha =. novaSenha]
                    setSession "_SENHA" novaSenha
                    redirect PerfilR
        _ -> redirect ErroR
    
postPedidoR :: Handler ()
postPedidoR = do
    pedido <- requireJsonBody :: Handler Pedido
    pedidoId <- runDB $ insert pedido
    sendResponse (object [pack "resp" .= pack (show $ fromSqlKey pedidoId)])