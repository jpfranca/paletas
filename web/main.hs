{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Yesod
import Yesod.Static
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Control.Applicative
import Import
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)

connStr = "dbname=d649v6hs211uh host=ec2-54-204-46-221.compute-1.amazonaws.com user=ywphdgpebmhtlx password=7HZV3l99vZecCXAL7yQLAjx7WI port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (WebSite t pool)