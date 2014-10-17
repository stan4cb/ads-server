{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified DB

import Model
import Data.Text
import Web.Spock.Simple

dPort = 3001
key_adID = "ad_id"

main = do
	DB.initDB
	pool <- DB.createPool
	DB.lockDB pool
	spock dPort sessCfg DB.gConnFake pool runUrl
	where
		sessCfg = SessionCfg "def" (0) 42 id

runUrl = do
	subcomponent "/ad_api" $ do
			post "/canShowAd" $ getState >>= canShowAdRQ
			post "/adClick"   $ adClickRQ
			post "/getImg"    $ getImage
			post "/getUrl"    $ getUrl
			post "/getAd"     $ getAdRQ

{- PAGES-}

adClickRQ = param key_adID >>= DB.incClick


getImage = do
	ad <- (param key_adID >>= DB.getAd)
	file "image/jpeg" $ unpack $ append "img/" (gFilePath $ DB.extract ad)


getUrl = do
	ad <- (param key_adID >>= DB.getAd)
	text (gUrl $ (DB.extract ad))


getAdRQ = do
	ad <- (param key_adID >>= DB.getAd)
	json $ DB.extract ad


canShowAdRQ p = do
	(Just aID :: Maybe Int) <- param key_adID

	DB.incRequest aID
	cShow <- param key_adID >>= DB.getAd >>= canShow

	DB.incShow cShow aID

	text $ if cShow then "true" else "false"
	where
		canShow adRaw = return $ if gCanShow ad
								 then isThatTime (gRequestCount ad) (gShowRatio ad)
								 else False
					where
						ad = DB.extract adRaw
						isThatTime 0 _       = False
						isThatTime _ 0       = False
						isThatTime total div = mod total div == 0