module Main
( main
) where

import System.Environment
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Network.Wai.Middleware.Cors
import Web.Scotty

import Routes (routes)

------------------------

default_port = 5000

getPort :: IO Int
getPort = fromMaybe default_port . (readMaybe =<<) <$> lookupEnv "PORT"

main :: IO ()
main = do
  port <- getPort
  scotty port $ do
    middleware simpleCors
    routes
