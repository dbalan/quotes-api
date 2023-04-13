{-# LANGUAGE OverloadedStrings #-}

module Parsers.Readwise where

import Data.Csv
import Data.Text
import Data.Vector (toList)
import Data.ByteString.Lazy (ByteString)
import Api.Types (Quote(..))

data RwHighlight = RwHighlight { rhHightlight :: Text
                               , rhTitle :: Text
                               , rhAuthor :: Text
                               , rhBookID :: Text
                               , rhNote :: Text
                               , rhColor :: Text
                               , rhTags :: Text
                               , rhLocationType :: Text
                               , rhLocation :: Text
                               , rhHighlighedAt :: Text
                               , rhDocumentTags :: Text
                               } deriving (Show, Eq)

instance FromNamedRecord RwHighlight where
  parseNamedRecord m = RwHighlight <$> m .: "Highlight"
                                    <*> m .: "Book Title"
                                    <*> m .: "Book Author"
                                    <*> m .: "Amazon Book ID"
                                    <*> m .: "Note"
                                    <*> m .: "Color"
                                    <*> m .: "Tags"
                                    <*> m .: "Location Type"
                                    <*> m .: "Location"
                                    <*> m .: "Highlighted at"
                                    <*> m .: "Document tags"

parseDocument :: ByteString -> Either String [RwHighlight]
parseDocument d = case decodeByName d of
  Left err -> Left err
  Right (_, va) -> Right $ toList va

parse :: ByteString -> Either String [Quote]
parse d = case parseDocument d of
  Left err -> Left err
  Right rw -> Right $ fmap (\r -> Quote { qQuote = rhHightlight r
                                  , qAuthor = rhAuthor r
                                  , qTitle = rhTitle r
                                  , qPage = rhLocation r
                                  , qChapter = Nothing
                                  , qCreatedOn = Nothing
                                }) rw
