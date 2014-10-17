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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Ad
  canShow Bool
  showRatio Int
  showCount Int
  clickCount Int
  requestCount Int
  imagePath Text
  clickUrl Text
|]

instance FromJSON Ad where
 parseJSON (Object v) =
    Ad <$> v .: "canShow"
       <*> v .: "showRatio"
       <*> v .: "showCount"
       <*> v .: "clickCount"
       <*> v .: "requestCount"
       <*> v .: "imagePath"
       <*> v .: "clickUrl"
 parseJSON _ = mzero

instance ToJSON Ad where
 toJSON (Ad canShow showRatio showCount clickCount requestCount imagePath clickUrl) =
    object [ "canShow"      .= canShow
           , "showRatio"    .= showRatio
           , "showCount"    .= showCount
           , "clickCount"   .= clickCount
           , "requestCount" .= requestCount
           , "imagePath"    .= imagePath
           , "clickUrl"     .= clickUrl]

gCanShow   (Ad canShow _ _ _ _ _ _) = canShow
gShowRatio (Ad _ showRatio _ _ _ _ _) = showRatio
gSCount    (Ad _ _ showCount _ _ _ _) = showCount

gRequestCount (Ad _ _ _ _ requestCount _ _) = requestCount
gFilePath     (Ad _ _ _ _ _ imagePath _) = imagePath
gUrl          (Ad _ _ _ _ _ _ clickUrl) = clickUrl