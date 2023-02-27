{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types
( Quote(..)
)
where

import Database.SQLite.Simple
import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Database.SQLite.Simple.ToField (ToField(toField))

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
