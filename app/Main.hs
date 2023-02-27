{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Network.Wai.Handler.Warp
import Data.Proxy
import Servant
import Control.Monad.IO.Class

import Api.Types
import qualified Parsers.KOReader as KO

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let dbfile = "quotes.db"
  initDb dbfile
  runApp dbfile


type API = "quotes" :> Get '[JSON] [Quote]
         :<|> "quote" :> "random" :> Get '[JSON] Quote
         :<|> "koreader" :> ReqBody '[JSON] KO.KoHighlight :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy

initDb :: FilePath -> IO ()
initDb dbFile = withConnection dbFile $ \conn ->
  execute_ conn
    [sql|CREATE TABLE IF NOT EXISTS quotes ( quote text non null
                                           , author text
                                           , title text
                                           , page text
                                           , chapter text
                                           , created_on integer);|]

-- | TODO: readerT
server :: FilePath -> Server API
server dbf = listQuotes dbf :<|> randomQuote dbf :<|> addKoReader dbf
-- | API begins here
randomQuote :: FilePath -> Handler Quote
randomQuote db = do
  qts <- (liftIO $ withConnection db $ \c -> query_ c qry)
  case (length qts) of
    0 -> undefined
    _ -> pure (head qts)
  where
    qry = [sql|SELECT * FROM quotes ORDER BY RANDOM();|]

listQuotes :: FilePath -> Handler [Quote]
listQuotes db = liftIO $ withConnection db $ \conn -> query_ conn [sql|SELECT * FROM quotes;|]

addKoReader :: FilePath -> KO.KoHighlight -> Handler NoContent
addKoReader db hl = do
  liftIO $ withConnection db $ \c ->
    executeMany c qry qts
  pure NoContent
  where
    qry = [sql|INSERT INTO quotes VALUES (?,?,?,?,?,?);|]
    qts = KO.parse hl

runApp :: FilePath -> IO ()
runApp dbfile = run 8081 (serve api $ server dbfile)
