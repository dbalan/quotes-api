{-# LANGUAGE QuasiQuotes #-}

module Database where

import Database.SQLite.Simple.QQ
import Database.SQLite.Simple

import Api.Types

initDb :: FilePath -> IO ()
initDb dbFile = withConnection dbFile $ \conn ->
  execute_ conn
    [sql|CREATE TABLE IF NOT EXISTS quotes ( quote text non null
                                           , author text
                                           , title text
                                           , page text
                                           , chapter text
                                           , created_on integer);|]

insertQts :: FilePath -> [Quote] -> IO ()
insertQts db qts = do
  withConnection db $ \c ->
    executeMany c qry qts
  where
    qry = [sql|INSERT INTO quotes VALUES (?,?,?,?,?,?);|]
