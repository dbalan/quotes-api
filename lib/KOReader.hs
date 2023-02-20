-- | Module to parse KOReader hightlight file
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module KOReader where

import Deriving.Aeson
import Data.Aeson
import Data.Text

showT :: Show a => a -> Text
showT = pack . show

newtype KoPage = KoPage Text
  deriving (Show, Eq, Generic)

instance FromJSON KoPage where
  parseJSON (Number s) = pure (KoPage $ showT $ round s)
  parseJSON (String s) = pure (KoPage (showT s))
  parseJSON _ = undefined

instance ToJSON KoPage where
  toJSON (KoPage a) = toJSON a

data KoHighlight = KoHighlight { khlDocuments :: [KoDocument]
                               , khlVersion :: Text
                               -- TODO: UNIX time
                               , khlCreatedOn :: Integer }
                  deriving (Show, Eq, Generic)
                  deriving (FromJSON, ToJSON)
                    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "khl", CamelToSnake]] KoHighlight

data KoDocument = KoDocument { kodTitle :: Text
                             , kodAuthor :: Maybe Text
                             , kodFile :: Maybe Text
                             , kodEntries :: [KoEntry] }
                  deriving (Show, Eq, Generic)
                  deriving (FromJSON, ToJSON)
                    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "kod", CamelToSnake]] KoDocument

data KoEntry = KoEntry { koeChapter :: Maybe Text
                       , koePage :: KoPage
                       -- TODO: unix time
                       , koeTime :: Maybe Integer
                       , koeSort :: Maybe Text
                       , koeDrawer :: Maybe Text
                       , koeText :: Text}
                  deriving (Show, Eq, Generic)
                  deriving (FromJSON, ToJSON)
                    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "koe", CamelToSnake]] KoEntry
