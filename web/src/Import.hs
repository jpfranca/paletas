{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Import where

import Yesod

pRoutes = [parseRoutes|
/ HomeR GET
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
/pedidoproduto PedidoProdutoR POST -- VERIFICADO PERMISSAO ATE AQUI
/pedidosolicitacao PedidoSolicitacaoR GET
/listapedido ListaPedidoR GET
/listapedido/#ClienteId ListaPedidoIdR GET
/erro ErroR GET 
/static StaticR Static getStatic
|]