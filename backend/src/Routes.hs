{-# LANGUAGE OverloadedStrings #-}

module Routes
( routes
) where

import Web.Scotty
import Network.HTTP.Types (notFound404)

import Story (_meta, allExampleStories, lookupStoryByID)

---------------------------

routes = do
  get "/stories"   stories
  get "/story/:id" story

---------------------------

stories = json $ _meta <$> allExampleStories

story = do
  reqID <- param "id" :: ActionM Int
  case lookupStoryByID reqID allExampleStories of
    Just stry -> json stry
    Nothing   -> status notFound404
