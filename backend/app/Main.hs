{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import System.Environment
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)

import Network.Wai.Middleware.Cors
import Web.Scotty

import Routes (routes)
import DB (dbtest)

------------------------

default_port = 5000

getPort :: IO Int
getPort = fromMaybe default_port . (readMaybe =<<) <$> lookupEnv "PORT"

server :: IO ()
server = do
  port <- getPort
  scotty port $ do
    middleware simpleCors
    routes

main = dbtest
