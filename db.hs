{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module DB where

import Model
import Data.Text hiding(count)
import Data.Aeson
import Database.Persist.Sqlite

{- --- -}
initDB = runSqlite "mydb.sqlite" $ runMigration migrateAll

runDB action = runSqlite "mydb.sqlite" $ action

{- Add -}
insertPerson person = runDB $ insert person

addMatch match = runDB $ insert match

{- Update -}
uMatchStat mid nStatus =
	runDB $ update (toKeyCP mid) [CPoolStatus =. nStatus]

uMatchTarget mid nStatus =
	runDB $ update (toKeyCP mid) [CPoolTargetScore =. nStatus]

uScore pid score =
	runDB $ update (toKeyLB pid) [LeaderboardScore =. score]

uName pid nName =
	runDB $ update (toKeyLB pid) [LeaderboardName =. nName]

suId pid = runDB $ update pid [LeaderboardDId =. (gInt $ keyOutLB pid)]

muId pid = runDB $ update pid [CPoolDId =. (gInt $ keyOutCP pid)]

{- Get -}
getTop = runDB $ selectList [] [Desc LeaderboardScore, LimitTo 10]

getTopChallenge = runDB $ selectList [] [Desc LeaderboardChallengeScore, LimitTo 10]

getByID (pid :: Int) = runDB $ selectList [LeaderboardId ==. (toKeyLB pid)] []

getMatch (pid :: Int) = runDB $ selectList [CPoolTo ==. pid] []

getByMatchID (pid :: Int) = runDB $ selectList [LeaderboardId ==. (toKeyLB pid)] []

getPlayerRank (score :: Int) = runDB $ count [LeaderboardScore >. score]

getPlayerRankC (score :: Int) = runDB $ count [LeaderboardChallengeScore >. score]

getPlayerRandom w = (runDB $ rawSql (getWithout w)  [])

getWithout w = append (append "select ?? from leaderboard where id!=" (pack $ show w)) " order by random() limit 1"

{- Remove -}
removeMatch (mID :: Int) = runDB $ delete $ (toKeyCP mID :: CPoolId) 

{- Tools -}
gInt (SqlBackendKey k) = fromIntegral k

--keyOut key = unKey key

keyOutLB (LeaderboardKey key) = key
keyOutCP (CPoolKey key) = key

--toKey key = (Key $ PersistInt64 $ fromIntegral key)
toKeyLB key = (LeaderboardKey $ fromIntegral key)
toKeyCP key = (CPoolKey $ fromIntegral key)

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e


{-
updateRowsByID pid rowToUpdate nVal =
	runDB $ update (Key $ toPersistValue pid) [rowToUpdate =. nVal]

updateRowsByName (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

extractKeys [] = []
extractKeys (e:ent) = eV : extractKeys ent
	where
		eV = keyOut e
-}