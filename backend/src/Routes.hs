{-# LANGUAGE OverloadedStrings #-}

module Routes
( routes
) where

import Data.Text
import Data.Text.Read (decimal)

import Web.Scotty
import Network.HTTP.Types (badRequest400, notFound404)

import Story

routes = do
  matchAny "/" $ text "Success"
  get "/stories" $ do
    json $ _meta <$> allExampleStories
  get "/story/:id" $ do
    reqID <- param "id" :: ActionM Text
    case decimal reqID of
      Right (reqIDint, "") -> case lookupByID reqIDint allExampleStories of
        Just stry -> json stry
        Nothing -> do
          status notFound404
          json $ ErrMsg $ "ID '" <> reqID <> "' not found!"
      Right (reqIDint, _) -> do
        status badRequest400
        json $ ErrMsg $ "bad request: " <> reqID
      Left _ -> do
        status badRequest400
        json $ ErrMsg $ "bad request: " <> reqID
