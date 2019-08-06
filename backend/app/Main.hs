
module Main
( main
) where

import System.Environment
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Routes (routes)
import DB (testDB, testDB2)

------------------------

default_port = 5000

getPort :: IO Int
getPort = fromMaybe default_port . (readMaybe =<<) <$> lookupEnv "PORT"

server :: IO ()
server = do
  port <- getPort
  scotty port $ do
    middleware simpleCors
    middleware logStdout
    routes

main = server
