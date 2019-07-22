{-# LANGUAGE OverloadedStrings #-}

module DB
( testDB
, testDB2
, selectStoryMetas
, selectStory
, withDB
) where

import Data.Maybe (fromMaybe, fromJust, listToMaybe)
import Data.String (fromString)
import Data.ByteString (ByteString)
import Control.Applicative (liftA3)
import System.Environment
import System.Exit (exitFailure)

import Database.PostgreSQL.Simple (connectPostgreSQL, query, query_, Connection, close)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.Types (Only(..))

import Story (Story(..), StoryMeta(..), story)

type DBAction a = Connection -> IO a

instance FromRow Story where
  fromRow = story <$> field <*> field <*> field

instance FromRow StoryMeta where
  fromRow = StoryMeta <$> field <*> field

selectStory :: Int -> Connection -> IO (Maybe Story)
selectStory storyID conn = do
  ss <- query conn "SELECT * FROM Story WHERE story_id=?" (Only storyID)
  return $ listToMaybe ss

selectStories :: Connection -> IO [Story]
selectStories conn = do
  query_ conn "SELECT * FROM Story"

selectStoryMetas :: Connection -> IO [StoryMeta]
selectStoryMetas conn = do
  query_ conn "SELECT story_id, title FROM Story"

(<<$>>) = fmap . fmap

withDB :: ByteString -> (Connection -> IO a) -> IO a
withDB dbConnString dbAction = do
    conn <- connectPostgreSQL dbConnString 
    result <- dbAction conn
    close conn
    return result
