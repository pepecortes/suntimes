module Params where

import Options.Applicative
import System.Directory ( getHomeDirectory )

data AppMode = FileInput FilePath | Interactive
  deriving Show

data Params = Params 
                FilePath
                AppMode
  deriving Show


cmdLineParser :: IO Params
cmdLineParser = do 
  parser <- inputIO
  let modifiers = fullDesc <> progDesc "Report sunrise/sunset times for the specified locations"
  execParser $ info parser modifiers

inputIO :: IO (Parser Params)
inputIO = do
  configFileParser <- configFileParserIO
  inputFileParser <- inputFileParserIO
  let rawParser = Params <$> configFileParser <*> (FileInput <$> inputFileParser <|> interactiveParser)
  return $ rawParser <**> helper 

configFileParserIO :: IO (Parser FilePath)
configFileParserIO = do
  home <- getHomeDirectory
  return $ strOption (
       long "config"
    <> short 'c'
    <> help "Alternate configuration file (default: $HOME/.suntimes/config)" 
    <> value (home ++ "/.suntimes/config")
    <> metavar "FILE_PATH"
    )

inputFileParserIO :: IO (Parser FilePath)
inputFileParserIO = do
  --home <- getHomeDirectory
  return $ strOption (
       long "file" 
    <> short 'f'
    <> help "Input file"
    <> metavar "FILE_PATH"
    )

interactiveParser :: Parser AppMode
interactiveParser = flag Interactive Interactive (long "interactive" <> short 'i' <> help "Interactive mode")