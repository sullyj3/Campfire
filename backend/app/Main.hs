
module Main
( main
) where

import System.Environment
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.ByteString (ByteString)

import Network.Wai.Middleware.Cors
import Web.Scotty

import Routes (routes)
import DB (testDB, testDB2)

------------------------

default_port = 5000

getPort :: IO Int
getPort = fromMaybe default_port . (readMaybe =<<) <$> lookupEnv "PORT"

getDbAddr :: IO ByteString
getDbAddr = do
  mdbURL <- fromString <<$>> lookupEnv "DATABASE_URL"
  case mdbURL of
	Just dbURL -> return dbURL
	Nothing -> do
	  putStrLn "You need to set the DATABASE_URL env variable!"
	  exitFailure

server :: IO ()
server = do
  port <- getPort
  dbAddr <- getDbAddr
  scotty port $ do
    middleware simpleCors
    routes dbAddr

main = server
