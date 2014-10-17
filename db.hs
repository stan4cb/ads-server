{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module DB where

import Model
import Data.Text hiding(count)
import Database.Persist.Sqlite

import Web.Spock.Simple
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class (liftIO)

dbName = "adser.sqlite"

gConnFake = PCConn (ConnBuilder (return ()) (const (return ())) (PoolCfg 1 1 1))

{- Main -}

initDB   = runSqlite dbName $ runMigration migrateAll
lockDB p = runSqlPool (selectFirst [AdId ==. (toKey (0 :: Int))] []) p

createPool = runNoLoggingT $ createSqlitePool dbName 10

runIOPool action = do
	pool <- getState
	liftIO $ runSqlPool action pool

{- Update -}

incClick Nothing = liftIO $ print ("Missing or invalid param" :: Text)
incClick (Just pid :: Maybe Int) = runIOPool (update (toKey pid) [AdClickCount +=. 1])

incShow False _  = liftIO $ return ()
incShow True pid = runIOPool (update (toKey pid) [AdShowCount +=. 1])

incRequest pid = runIOPool (update (toKey pid) [AdRequestCount +=. 1])
{- Get -}

getAd Nothing                 = liftIO $ return Nothing
getAd (Just pid :: Maybe Int) = runIOPool (selectFirst [AdId ==. (toKey pid) ] [])


{- Tools -}

gInt (SqlBackendKey k) = fromIntegral k

toKey key = (AdKey $ fromIntegral key)

--extract Nothing  = Nothing
extract (Just e) = entityVal e

extractS [] = []
extractS (e:es) = extract e : extractS es