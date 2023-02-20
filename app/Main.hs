{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Text (Text)
import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Network.Wai.Handler.Warp
import Deriving.Aeson
import Data.Proxy
import Servant

import Control.Monad.IO.Class

data Quote = Quote { qQuote :: Text
                   , qAuthor :: Text
                   , qTitle :: Text
                   -- , qPage :: Text
                   -- , qChapter :: Text
                   -- , qTime :: UnixTime
                   } deriving (Show, Eq, Ord, Generic)
                     deriving (FromJSON,ToJSON)
                       via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "q", CamelToSnake]] Quote

instance FromRow Quote where
  fromRow = Quote <$> field <*> field <*> field


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let dbfile = "quotes.db"
  initDb dbfile
  runApp dbfile


type API = Get '[JSON] [Quote]

api :: Proxy API
api = Proxy

initDb :: FilePath -> IO ()
initDb dbFile = withConnection dbFile $ \conn ->
  execute_ conn
    [sql|CREATE TABLE IF NOT EXISTS quotes (quote text non null, author text, book text)|]

server :: FilePath -> Server API
server dbf = listQuotes dbf


listQuotes :: FilePath -> Handler [Quote]
listQuotes db = liftIO $ withConnection db $ \conn -> query_ conn [sql|SELECT * FROM quotes;|]

runApp :: FilePath -> IO ()
runApp dbfile = run 8081 (serve api $ server dbfile)
