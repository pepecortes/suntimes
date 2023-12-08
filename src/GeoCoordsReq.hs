{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeoCoordsReq where

import STExcept
import Types

import Network.HTTP.Req
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader ( MonadReader(ask) )
import UnliftIO



--getCoords :: (MonadReader SunConfig m, MonadIO m, MonadCatch m) => Address -> m GeoCoords
getCoords :: (MonadReader SunConfig m, MonadUnliftIO m) => Address -> m GeoCoords
getCoords addr = handle rethrowReqException $  do
  SunConfig {..} <- ask
  let
    ep = https "nominatim.openstreetmap.org" /: "search"
    reqParams =
      mconcat [
        "q" =: addr
        , "format" =: ("json" :: T.Text)
        , "limit" =: (1 :: Int)
        , "email" =: email auth
        , header "User-Agent" (encodeUtf8 $ agent auth)
        ]
    request = req GET ep NoReqBody jsonResponse reqParams
  res <- responseBody <$> runReq defaultHttpConfig request
  case res of
    --[] -> throwM (UnknownLocation addr)
    [] -> throwIO (UnknownLocation addr)
    (coords:_) -> pure coords
