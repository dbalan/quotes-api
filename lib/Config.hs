{-# LANGUAGE OverloadedStrings #-}
module Config
  ( parserOpts
  , AppConfig(..)
  , importerParserOpts
  , ImporterConfig(..)
  ) where

import Options.Applicative

data AppConfig = AppConfig
  { appPort :: Int
  , appDbFile :: FilePath
  } deriving (Show, Eq)

appConfig :: Parser AppConfig
appConfig = AppConfig
  <$> option auto
     ( long "port"
     <> help "port to listen"
     <> showDefault
     <> value 8000
     <> metavar "INT")
   <*> strOption
     ( long "dbpath"
     <> help "sqlite db file path"
     <> showDefault
     <> value "quotes.db"
     <> metavar "TARGET")

parserOpts :: ParserInfo AppConfig
parserOpts =  info (appConfig <**> helper)
      ( fullDesc
     <> progDesc "Serve Quotes API"
     <> header "quotes api" )

data ImporterConfig = ImporterConfig
  { ioAppDbFile :: FilePath
  , ioReadwiseFile :: FilePath
  } deriving (Show, Eq)

importerConfig :: Parser ImporterConfig
importerConfig = ImporterConfig
  <$> strOption
     ( long "dbpath"
     <> help "sqlite db file path"
     <> showDefault
     <> value "quotes.db"
     <> metavar "TARGET")
    <*> strOption
     ( long "readwise"
     <> help "readwise export file path"
     <> showDefault
     <> value "readwise.csv"
     <> metavar "RWCSV")

importerParserOpts :: ParserInfo ImporterConfig
importerParserOpts = info (importerConfig <**> helper)
  ( fullDesc
  <> progDesc "Import data into db"
  <> header "importer API")
