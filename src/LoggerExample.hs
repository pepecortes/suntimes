{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module LoggerExample where

import Control.Monad.Logger
import Control.Monad.State
import Data.Text (pack)

popAndLog :: LoggingT (StateT [Int] IO) ()
popAndLog = do
  _:xs <- lift get
  lift (put xs)
  $logDebug ("***" <> pack (show xs) <> "***")

logStateEx :: LoggingT (StateT [Int] IO) Int
logStateEx =  do
  popAndLog
  popAndLog
  pure 5

main2 :: IO ()
main2 = runStateT (runStdoutLoggingT logStateEx) [1,2,3] >>= print