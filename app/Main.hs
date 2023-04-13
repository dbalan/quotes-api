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
import Data.Text.Encoding (decodeUtf8)
import Crypto.Argon2
import Data.Text.Short (fromText, ShortText)
import Data.ByteString (ByteString)

import Api.Types
import qualified Parsers.KOReader as KO
import Config
import Options.Applicative
import Database

data User = User
  { username :: T.Text
  , password :: ByteString
  } deriving (Show, Eq)

type API = Get '[HTML] Quote
         :<|> "quotes" :> Get '[JSON] [Quote]
         :<|> "quote" :> "random" :> Get '[JSON] Quote
         :<|> "today" :> Get '[HTML] Quote
         :<|> BasicAuth "update data" User :> "koreader" :> ReqBody '[JSON] KO.KoHighlight :> Post '[JSON] NoContent


api :: Proxy API
api = Proxy

checkBasicAuth :: T.Text -> ShortText -> BasicAuthCheck User
checkBasicAuth user passhash = BasicAuthCheck $ \authData ->
  let u =  decodeUtf8 (basicAuthUsername authData)
      p = basicAuthPassword authData
  in
    case user == u of
      False -> return NoSuchUser
      True -> case verifyEncoded passhash p of
                 Argon2Ok -> return $ Authorized $ User u p
                 _ -> return Unauthorized

-- | TODO: readerT
server :: FilePath -> Server API
server dbf = randomQuote dbf
      :<|> listQuotes dbf
      :<|> randomQuote dbf
      :<|> randomQuote dbf
      :<|> const (addKoReader dbf)

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

runApp :: AppConfig -> IO ()
runApp c = run (appPort c) (serveWithContext api ctx $ server (appDbFile c))
  where
    ctx = checkBasicAuth (appUser c) (fromText $ appPassHash c):. EmptyContext

main :: IO ()
main = do
  conf <- execParser parserOpts
  putStrLn $ "running with conf" <> show conf
  initDb (appDbFile conf)
  runApp conf
