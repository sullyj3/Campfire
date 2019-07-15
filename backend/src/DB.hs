{-# LANGUAGE OverloadedStrings #-}

module DB
( testDB
, testDB2
, getDB
, selectStoryMetas
) where

import Data.Maybe (fromMaybe, fromJust)
import Data.String (fromString)
import Data.ByteString (ByteString)
import Control.Applicative (liftA3)
import System.Environment
import System.Exit (exitFailure)

import Database.PostgreSQL.Simple (connectPostgreSQL, query_, Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.Types (Only(..))

import Story (Story(..), StoryMeta(..), story)

instance FromRow Story where
  fromRow = story <$> field <*> field <*> field

instance FromRow StoryMeta where
  fromRow = StoryMeta <$> field <*> field

selectStories :: Connection -> IO [Story]
selectStories conn = do
  query_ conn "SELECT * FROM Story"

selectStoryMetas :: Connection -> IO [StoryMeta]
selectStoryMetas conn = do
  query_ conn "SELECT story_id, title FROM Story"

(<<$>>) = fmap . fmap

dbConnString :: IO ByteString
dbConnString = do
  mdbURL <- fromString <<$>> lookupEnv "DATABASE_URL"
  case mdbURL of
    Just dbURL -> return dbURL
    Nothing -> do
      putStrLn "You need to set the DATABASE_URL env variable!"
      exitFailure

testDB :: IO ()
testDB = do
  putStrLn "testing db: should print story metadata"
  getDB
    >>= selectStoryMetas
    >>= print

testDB2 :: IO ()
testDB2 = do
  putStrLn "testing db: should print all stories"
  getDB
    >>= selectStories
    >>= print


getDB :: IO Connection
getDB = dbConnString >>= connectPostgreSQL
