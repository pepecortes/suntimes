{-# LANGUAGE RecordWildCards #-}

module ProcessRequest where

import App ( SuntimesStack )
import GeoCoordsReq ( getCoords )
import Types
    ( LocalTimes(formattedTime, zoneName),
      SunTimes(SunTimes, sunrise, sunset) )
import SunTimes ( getSunTimesUTC, toLocalTimes )
import STExcept ( SunInfoException(NetworkError, ServiceAPIError) )

import Data.Text ( Text, isPrefixOf, pack )
import Fmt ((+|), (|+), Builder, (+||))
import Data.Foldable (foldrM)
import UnliftIO (catch, finally, liftIO, handle)
import UnliftIO.Concurrent (threadDelay)


processSingleRequest :: Text -> SuntimesStack Builder
processSingleRequest location | "#" `isPrefixOf` location = pure ""
                              | otherwise = processSingleRequest' location
  where
    processSingleRequest' :: Text -> SuntimesStack Builder
    processSingleRequest' loc = do
      coords <- getCoords loc
      SunTimes{..} <- getSunTimesUTC coords
      localRise <- toLocalTimes coords sunrise
      localSet <- toLocalTimes coords sunset
      let out = "\n-------------------\n"+|loc|+" ("+|zoneName localRise|+
                ")\nsunrise: "+|formattedTime localRise|+
                "\nsunset: "+|formattedTime localSet|+
                "\n"
      pure out

processRequests :: [Text] -> SuntimesStack Builder
processRequests = foldrM process ""
  where
    process :: Text -> Builder -> SuntimesStack Builder
    process t acc = fmap (+|| acc) $ processSingleRequest t
                    `catch` handler
                    `finally` threadDelay (3 * 1000000) -- prevent API request rate limit
    handler :: SunInfoException -> SuntimesStack Builder
    handler e = pure $ "error: "+|show e|+"\n"

processInteractive :: SuntimesStack Builder
processInteractive = handle handler $ do
  liftIO $ putStrLn "Enter location: "
  txt <- liftIO getLine
  processSingleRequest $ pack txt
  where
    handler :: SunInfoException -> SuntimesStack Builder
    handler e@(ServiceAPIError _) = pure $ "error: "+|show e|+"\n"
    handler e@(NetworkError _) = pure $ "error: "+|show e|+"\n"
    handler e = do
      yesno <- liftIO $ 
           putStrLn ("error: " ++ show e)
        >> putStr "Try again? " 
        >> getLine
      if yesno `elem` ["y", "Y", "yes"]
      then processInteractive
      else pure "bye"
      
  
  


