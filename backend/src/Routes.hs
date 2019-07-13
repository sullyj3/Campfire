{-# LANGUAGE OverloadedStrings #-}

module Routes
( routes
) where

import Data.Text
import Data.Text.Read (decimal)

import Web.Scotty
import Network.HTTP.Types (badRequest400, notFound404)

import Story (_meta, ErrMsg(..), allExampleStories, lookupStoryByID)

---------------------------

routes = do
  get "/stories"   stories
  get "/story/:id" story

---------------------------

stories = json $ _meta <$> allExampleStories

story = do
  reqID <- param "id" :: ActionM Text
  case decimal reqID of
    Right (reqIDint, "") -> case lookupStoryByID reqIDint allExampleStories of
      Just stry -> json stry
      Nothing -> do
        status notFound404
        json $ ErrMsg $ "ID '" <> reqID <> "' not found!"
    Right (reqIDint, _) -> badRequest reqID
    Left _              -> badRequest reqID

badRequest req = do
  status badRequest400
  json $ ErrMsg $ "bad request: " <> req
