-- | Module to parse KOReader hightlight file
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsers.KOReader
( KoHighlight(..)
, parse
)
where

import Deriving.Aeson
import Data.Aeson
import Data.Text hiding (concatMap)
import Data.Maybe (fromMaybe)

import Api.Types (Quote(..))

showT :: Show a => a -> Text
showT = pack . show

newtype KoPage = KoPage Text
  deriving (Eq, Generic)

instance Show KoPage where
  show (KoPage a) = Data.Text.unpack a

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

parse :: KoHighlight -> [Quote]
parse khl = concatMap parseDocument (khlDocuments khl)

parseDocument :: KoDocument -> [Quote]
parseDocument kd = (parseEntry title author) <$> (kodEntries kd)
  where
    title = kodTitle kd
    author = kodAuthor kd

parseEntry :: Text -> Maybe Text -> KoEntry -> Quote
parseEntry title author ke = Quote { qQuote = koeText ke
                      , qAuthor = fromMaybe "" author
                      , qTitle = title
                      , qPage = showT $ koePage ke
                      , qChapter = koeChapter ke
                      , qCreatedOn = koeTime ke
                      }

