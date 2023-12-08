{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App ( runSuntimesStack )
import Params ( Params(..), AppMode(FileInput, Interactive), cmdLineParser )
import Types ( SunConfig )
import STExcept ( SunInfoException(ConfigError, UnknownLocation) )
import ProcessRequest ( processInteractive, processRequests )

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (decode)
import UnliftIO ( handle, throwIO )
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Fmt (fmt)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)


main :: IO ()
main =  handle dealWithException $ do
  params@(Params cfgFilePath _) <- cmdLineParser
  cfg <- loadConfig cfgFilePath
  work cfg params
  

dealWithException :: HasCallStack => SunInfoException -> IO ()
dealWithException (UnknownLocation _) = error "cagüen tó"
dealWithException e = do
  putStrLn (prettyCallStack callStack)
  print ("he capturado un pez-ception: " ++ show e)
  return ()


loadConfig :: FilePath -> IO SunConfig
loadConfig fpath = do
  bs <- BL.readFile fpath
  case decode bs of
    Nothing -> throwIO ConfigError
    Just cfg -> pure cfg


work :: SunConfig -> Params -> IO ()
work cfg (Params _ Interactive) = runSuntimesStack cfg processInteractive >>= putStrLn . fmt
work cfg (Params _ (FileInput fpath)) = do
  contents <- BL.readFile fpath
  let lineas = map (toStrict . decodeUtf8) $ BL.lines contents
  runSuntimesStack cfg (processRequests lineas) >>= putStrLn . fmt