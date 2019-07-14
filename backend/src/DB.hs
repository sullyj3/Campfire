{-# LANGUAGE OverloadedStrings #-}

module DB
( dbtest
) where

import Data.Maybe (fromMaybe, fromJust)
import Data.String (fromString)
import System.Environment

import Database.PostgreSQL.Simple

select4 :: String -> String -> IO Int
select4 url pass = do
  conn <- connectPostgreSQL $ mconcat 
    [ "host="
    , fromString url
    , " port=5432 dbname=dvdrental connect_timeout=10 password="
    , fromString pass ]
  [Only i] <- query_ conn "select 2 + 2"
  return i

dbtest :: IO ()
dbtest = do
  pgPass <- fromJust <$> lookupEnv "PGPASS"
  pgURL <- fromMaybe "localhost" <$> lookupEnv "DATABASE_URL" :: IO String
  select4 pgURL pgPass >>= print

