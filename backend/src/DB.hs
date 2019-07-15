{-# LANGUAGE OverloadedStrings #-}

module DB
( dbtest
) where

import Data.Maybe (fromMaybe, fromJust)
import Data.String (fromString)
import Data.ByteString (ByteString)
import System.Environment
import System.Exit (exitFailure)

import Database.PostgreSQL.Simple

select4 :: ByteString -> IO Int
select4 connString = do
  conn <- connectPostgreSQL connString
  [Only i] <- query_ conn "select 2 + 2"
  return i

(<<$>>) = fmap . fmap

dbConnString :: IO ByteString
dbConnString = do
  mdbURL <- fromString <<$>> lookupEnv "DATABASE_URL"
  case mdbURL of
    Just dbURL -> return dbURL
    Nothing -> do
      putStrLn "You need to set the DATABASE_URL env variable!"
      exitFailure

dbtest :: IO ()
dbtest = do
  putStrLn "testing db: should print 4"
  dbConnString >>= select4 >>= print
