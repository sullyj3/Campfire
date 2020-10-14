{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import Data.List (find)

import System.Environment
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)

import Network.Wai
import Network.HTTP.Types.Header
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Routes (routes)

import Debug.Trace (trace)

------------------------

defaultPort = 5000

getPort :: IO Int
getPort = fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv "PORT"

server :: IO ()
server = do
  port <- getPort
  scotty port $ do
    middleware $ cors myCORSPolicy
    middleware logStdout
    routes

isJson :: Request -> Bool
isJson r = isJust $ find (==(hContentType,"application/json")) (requestHeaders r)

myCORSPolicy :: Request -> Maybe CorsResourcePolicy
myCORSPolicy r = Just $
    CorsResourcePolicy
          { corsOrigins = Nothing
          --corsOrigins = Just (["http://localhost:3000", "https://eager-hamilton-35aa59.netlify.com"], False)
          , corsMethods = ["GET", "POST"]
          , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
          , corsExposedHeaders = Nothing
          , corsMaxAge = Nothing
          , corsVaryOrigin = False
          , corsRequireOrigin = False
          , corsIgnoreFailures = False
          }

main = server
