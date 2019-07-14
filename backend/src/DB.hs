{-# LANGUAGE OverloadedStrings #-}

module DB
( dbtest
) where

import Data.Maybe (fromJust)
import Data.String (fromString)
import System.Environment

import Database.PostgreSQL.Simple

select4 :: String -> IO Int
select4 pass = do
  conn <- connectPostgreSQL $ "host=localhost port=5432 dbname=dvdrental connect_timeout=10 password=" <> fromString pass
  [Only i] <- query_ conn "select 2 + 2"
  return i

dbtest :: IO ()
dbtest = do
  pgPass <- fromJust <$> lookupEnv "PGPASS"
  select4 pgPass >>= print
  return ()

