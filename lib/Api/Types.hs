{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.Types
( Quote(..)
)
where

import Database.SQLite.Simple
import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Database.SQLite.Simple.ToField (ToField(toField))
import Text.Blaze
import qualified Text.Blaze.Html5 as HS
import qualified Text.Blaze.Html5.Attributes as HSA

data Quote = Quote { qQuote :: Text
                   , qAuthor :: Text
                   , qTitle :: Text
                   , qPage :: Text
                   , qChapter :: Maybe Text
                   , qCreatedOn :: Maybe Integer
                   } deriving (Show, Eq, Ord, Generic)
                     deriving (FromJSON,ToJSON)
                       via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "q", CamelToSnake]] Quote

instance FromRow Quote where
  fromRow = Quote <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Quote where
  toRow q = [toField (qQuote q),
             toField (qAuthor q),
             toField (qTitle q),
             toField (qPage q),
             toField (qChapter q),
             toField (qCreatedOn q)]

instance ToMarkup Quote where
  toMarkup q = HS.html $ do
    HS.head $ do
      HS.title "random quote"
      HS.link ! HSA.rel "stylesheet" ! HSA.type_ "text/css" ! HSA.href "https://unpkg.com/tachyons@4.12.0/css/tachyons.min.css"
    HS.body ! HSA.class_ "w-100 sans-serif" $ do
      HS.div ! HSA.class_ "fl f5 pa4 w-100" $ HS.a ! HSA.href "https://git.planet-express.in/dbalan/quotes-api" $ "built with quotes-api"
      HS.div ! HSA.class_ "fl center pa4" $ do
        HS.div ! HSA.class_ "f2 f1-ns measure fw7 lh-title mt0" $ HS.toHtml (qQuote q)
        HS.div ! HSA.class_ "f3 fl w-80" $ HS.toHtml (qAuthor q)
        HS.div ! HSA.class_ "f4 fl w-80" $ HS.toHtml $ qTitle q
