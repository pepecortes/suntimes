{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import Types

import Control.Monad.Reader
import Control.Monad.Logger
import UnliftIO (MonadUnliftIO)

newtype SuntimesStack a  = SuntimesStack { getTransformer :: ReaderT SunConfig (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SunConfig, MonadUnliftIO, MonadLogger)
  
runSuntimesStack :: SunConfig -> SuntimesStack a -> IO a
runSuntimesStack cfg stack = runStdoutLoggingT $ runReaderT (getTransformer stack) cfg
