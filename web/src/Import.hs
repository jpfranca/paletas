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
/produto/#ProdutoId ProdutoIdR POST
/cliente ClienteR GET
/cliente/#ClienteId ClienteIdR GET
/perfil PerfilR GET
/perfil/alteradados/#ClienteId PerfilAlteraDadosR POST
/perfil/alterasenha/#ClienteId PerfilAlteraSenha POST
/pedido PedidoR POST
/pedidoproduto PedidoProdutoR POST
/erro ErroR GET 
/static StaticR Static getStatic
|]