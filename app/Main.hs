{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Database.SQLite.Simple hiding ((:.))
import Database.SQLite.Simple.QQ
import Network.Wai.Handler.Warp
import Data.Proxy
import Servant
import Control.Monad.IO.Class
import Servant.HTML.Blaze
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

import Api.Types
import qualified Parsers.KOReader as KO
import qualified Parsers.Readwise as RW
import Config
import Options.Applicative
import Database

type API = Get '[HTML] Quote
         :<|> "quotes" :> Get '[JSON] [Quote]
         :<|> "quote" :> "random" :> Get '[JSON] Quote
         :<|> "today" :> Get '[HTML] Quote


api :: Proxy API
api = Proxy

-- | TODO: readerT
server :: FilePath -> Server API
server dbf = randomQuote dbf
      :<|> listQuotes dbf
      :<|> randomQuote dbf
      :<|> randomQuote dbf

-- | API begins here
randomQuote :: FilePath -> Handler Quote
randomQuote db = do
  qts <- liftIO $ withConnection db $ \c -> query_ c qry
  case length qts of
    0 -> undefined
    _ -> pure (head qts)
  where
    qry = [sql|SELECT * FROM quotes ORDER BY RANDOM();|]

listQuotes :: FilePath -> Handler [Quote]
listQuotes db = liftIO $ withConnection db $ \conn -> query_ conn [sql|SELECT * FROM quotes;|]

addKoReader :: FilePath -> KO.KoHighlight -> Handler NoContent
addKoReader db hl = do
  liftIO $ insertQts db (KO.parse hl)
  pure NoContent

addReadwise :: FilePath -> T.Text -> Handler NoContent
addReadwise db hl = do
  let
    qts = RW.parse (fromStrict $ encodeUtf8 hl)
  liftIO $ print $ show qts
  pure NoContent

runApp :: AppConfig -> IO ()
runApp c = run (appPort c) (serve api $ server (appDbFile c))

main :: IO ()
main = do
  conf <- execParser parserOpts
  putStrLn $ "running with conf" <> show conf
  initDb (appDbFile conf)
  runApp conf
