{-# LANGUAGE OverloadedStrings #-}

module DB
( testDB
, testDB2
, selectStoryMetas
, selectStory
, withDB
) where

import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.ByteString (ByteString)
import Control.Applicative (liftA3)
import System.Environment
import System.Exit (exitFailure)

import Database.PostgreSQL.Simple (connectPostgreSQL, query, query_, Connection, close)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.Types (Only(..))

import Story (Story(..), StoryMeta(..))

instance FromRow Story where
  fromRow = Story <$> field <*> field <*> field

instance FromRow StoryMeta where
  fromRow = StoryMeta <$> field <*> field

selectStory :: Int -> Connection -> IO (Maybe Story)
selectStory storyID conn = do
  ss <- query conn "SELECT * FROM Story WHERE story_id=?" (Only storyID)
  return $ listToMaybe ss

selectStories :: Connection -> IO [Story]
selectStories conn = query_ conn "SELECT * FROM Story"

selectStoryMetas :: Connection -> IO [StoryMeta]
selectStoryMetas conn = query_ conn "SELECT story_id, title FROM Story"

(<<$>>) = fmap . fmap


testDB :: IO ()
testDB = do
  putStrLn "testing db: should print story metadata"
  withDB selectStoryMetas >>= print

testDB2 :: IO ()
testDB2 = do
  putStrLn "testing db: should print all stories"
  withDB selectStories >>= print

-- TODO: this accesses the DATABASE_URL env variable every time. Seems like we
-- should read it once at the start of the program and then pass it around
withDB :: (Connection -> IO a) -> IO a
withDB dbAction = do
    conn <- connectPostgreSQL =<< dbConnString 
    result <- dbAction conn
    close conn
    return result
  where
    dbConnString :: IO ByteString
    dbConnString = do
      mdbURL <- fromString <<$>> lookupEnv "DATABASE_URL"
      case mdbURL of
        Just dbURL -> return dbURL
        Nothing -> do
          putStrLn "You need to set the DATABASE_URL env variable!"
          exitFailure
