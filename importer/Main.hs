module Main where

import Options.Applicative
import qualified Data.ByteString.Lazy as BSL

import Config
import Database

import qualified Parsers.Readwise as RW

runImporter :: FilePath -> FilePath -> IO ()
runImporter db rw = do
  x <- BSL.readFile rw
  let y = RW.parse x
  case y of
    Left err -> print err
    Right qts -> insertQts db qts

main :: IO ()
main = do
  conf <- execParser importerParserOpts
  initDb (ioAppDbFile conf)
  runImporter (ioAppDbFile conf) (ioReadwiseFile conf)
