{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Text ( Text, pack )
import GHC.Generics ( Generic )
import Data.Time (Day, formatTime, defaultTimeLocale, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import Data.Aeson ( FromJSON, ToJSON, withObject, (.:), parseJSON )

-- AESON, JSON handling cheatsheet
-- https://www.williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html


type Address = Text

data When = Now | On Day
  deriving Show

data GeoCoords = GeoCoords { lat :: Text,
                             lon :: Text }
  deriving (Show, Generic, FromJSON, ToJSON)

data SunTimes = SunTimes { sunrise :: UTCTime,
                           sunset :: UTCTime }
  deriving (Show, Generic, FromJSON, ToJSON)

data WebAPIAuth = WebAPIAuth { timeZoneDBkey :: Text
                             , email :: Text
                             , agent :: Text
                             , timezoneAPIkey :: Text }
  deriving (Show, Generic, FromJSON, ToJSON)


data LocalTimes = LocalTimes  { zoneName :: Text
                              , abbreviation :: Text
                              , daylightSaving :: Bool
                              , posixTime :: POSIXTime
                              , formattedTime :: Text
                              }
  deriving (Show)

instance FromJSON LocalTimes where
  parseJSON = withObject "LocalTimes" $ \obj -> do
    zName <- obj .: "zoneName"
    timeZone <- obj .: "abbreviation"
    dst <- obj .: "dst"
    psxTime <- obj .: "timestamp"
    let timeStr = formatTime defaultTimeLocale ("%d-%b-%Y %H:%M:%S " ++ timeZone) $ posixSecondsToUTCTime psxTime
    return (LocalTimes { zoneName = zName
                       , abbreviation = pack timeZone
                       , daylightSaving = (dst :: String) == "1"
                       , posixTime = psxTime
                       , formattedTime = pack timeStr
                       })

newtype SunConfig = SunConfig { auth :: WebAPIAuth }
  deriving (Generic, FromJSON, ToJSON)


