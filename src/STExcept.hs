{-# LANGUAGE DeriveAnyClass #-}

module STExcept where

import Types

import Data.Text (Text)
--import Control.Monad.Catch
import UnliftIO
import Network.HTTP.Req
import qualified Network.HTTP.Client as NC


data RequestError = EmptyRequest | WrongDay Text
  deriving Show

data SunInfoException = UnknownLocation Text
                      | UnknownTime GeoCoords
                      | FormatError RequestError
                      | ServiceAPIError String
                      | NetworkError SomeException
                      | ConfigError
   deriving Exception

instance Show SunInfoException where
  show (UnknownLocation loc) = show loc ++ ": Failed while determining coordinates"
  show (UnknownTime _) = "Failed while determining sunrise/sunset times"
  show (FormatError er) = show er
  show (ServiceAPIError _) = "Error while communicating with external services"
  show (NetworkError _) = "Network communication error"
  show ConfigError = "Error parsing configuration file"

rethrowReqException :: (MonadIO m) => HttpException -> m a
rethrowReqException (JsonHttpException s) = throwIO (ServiceAPIError s)
rethrowReqException (VanillaHttpException (
                        NC.HttpExceptionRequest _
                          (NC.StatusCodeException resp _ ))) =
  throwIO (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throwIO (NetworkError $ toException e)


{- rethrowReqException :: MonadThrow m => HttpException -> m a
rethrowReqException (JsonHttpException s) = throwM (ServiceAPIError s)
rethrowReqException (VanillaHttpException (
                        NC.HttpExceptionRequest _
                          (NC.StatusCodeException resp _ ))) =
  throwM (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throwM (NetworkError $ toException e) -}



{- testCatch :: String -> IO ()
testCatch arg = do
  computation <- handle dealWithFailure (aComputation arg)
  putStrLn computation
  where
    dealWithFailure :: SunInfoException -> IO String
    dealWithFailure ConfigError = pure "he atrapado un config error"
    dealWithFailure _ = pure "podria ser peor"

aComputation :: String -> IO String
aComputation str = do
  --let res = fuenteDeErrores str
  res <- fuenteDeErroresIO str
  pure $ "whatever" ++ res

fuenteDeErrores :: String -> String
fuenteDeErrores "c" = throw ConfigError
fuenteDeErrores "l" = throw $ UnknownLocation "muylejos"
fuenteDeErrores _ = "all ok"

fuenteDeErroresIO :: String -> IO String
fuenteDeErroresIO "c" = throw ConfigError
fuenteDeErroresIO "l" = throw $ UnknownLocation "muylejos"
fuenteDeErroresIO _ = pure "all ok" -}