{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SunTimes where

import Types
import STExcept

import Network.HTTP.Req
import Data.Aeson ( (.:), withObject, Value )
import Data.Aeson.Types (parseMaybe)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Control.Monad.Reader ( MonadReader(ask) )
--import Control.Monad.Catch (MonadThrow (throwM), handle, MonadCatch)
import UnliftIO


parseJsonValue :: Value -> Maybe SunTimes
parseJsonValue = parseMaybe $
  withObject "SunTimes" $ \obj -> do
    nested <- obj .: "results"
    sunrise <- nested .: "sunrise"
    sunset <- nested .: "sunset"
    return (SunTimes sunrise sunset)


getSunTimesUTC :: (MonadUnliftIO m) => GeoCoords -> m SunTimes
getSunTimesUTC coords@(GeoCoords lat lng) = handle rethrowReqException $  do
  let
    ep = https "api.sunrise-sunset.org" /: "json"
    reqParams = mconcat [ "lat" =: lat, "lng" =: lng, "formatted" =: (0 :: Int)]
    request = req GET ep NoReqBody jsonResponse reqParams
  value <- responseBody <$> runReq defaultHttpConfig request
  case parseJsonValue value of
    Nothing -> throwIO (UnknownTime coords)
    Just times -> pure times


toLocalTimes :: (MonadUnliftIO m, MonadReader SunConfig m) => GeoCoords -> UTCTime -> m LocalTimes
toLocalTimes (GeoCoords lat lng) time = do
  SunConfig {..} <- ask
  let
    ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
    reqParams = mconcat [ "key" =: timezoneAPIkey auth
                        , "lat" =: lat
                        , "lng" =: lng
                        , "time" =: formatTime defaultTimeLocale "%s" time
                        , "format" =: ("json" :: T.Text)
                        , "by" =: ("position" :: T.Text)
                        ]
    request = req GET ep NoReqBody jsonResponse reqParams
  responseBody <$> runReq defaultHttpConfig request
    
