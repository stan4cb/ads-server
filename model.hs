{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative
import Database.Persist.TH

data UserID = UserID { uID :: Int}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Leaderboard
  dId Int
  name Text
  score Int
  challengeScore Int
  deriving Show
CPool
  dId Int
	from Int
	to Int
	targetScore Int
  theme Int
  status Int
|]

instance FromJSON Leaderboard where
 parseJSON (Object v) =
    Leaderboard <$> v .: "dId"
                <*> v .: "name"
                <*> v .: "score"
                <*> v .: "challengeScore"
 parseJSON _ = mzero

instance ToJSON Leaderboard where
 toJSON (Leaderboard dId name score challengeScore) =
    object [ "dId"            .= dId
           , "name"           .= name
           , "score"          .= score 
           , "challengeScore" .= challengeScore ]

instance FromJSON CPool where
 parseJSON (Object v) =
  CPool <$> v .: "dId"
        <*> v .: "from"
        <*> v .: "to"
        <*> v .: "targetScore"
        <*> v .: "theme"
        <*> v .: "status"
 parseJSON _ = mzero

instance ToJSON CPool where
 toJSON (CPool dId from to targetScore theme status) =
    object [ "dId"         .= dId
           , "from"        .= from
           , "to"          .= to
           , "targetScore" .= targetScore
           , "theme"       .= theme
           , "status"      .= status]

instance FromJSON UserID where
 parseJSON (Object v) = UserID <$> v .: "uID"
 parseJSON _ = mzero

instance ToJSON UserID where
 toJSON (UserID uid) =
    object [ "uID" .= uid]
