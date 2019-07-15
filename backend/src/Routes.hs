{-# LANGUAGE OverloadedStrings #-}

module Routes
( routes
) where

import Control.Monad.IO.Class

import Web.Scotty
import Network.HTTP.Types (notFound404)

import Story (_meta, allExampleStories, lookupStoryByID)
import DB (getDB, selectStoryMetas)

---------------------------

routes = do
  get "/stories"   stories
  get "/story/:id" story

---------------------------

stories :: ActionM ()
stories = do
  storyMetas <- liftIO $ getDB >>= selectStoryMetas
  json $ storyMetas

story :: ActionM ()
story = do
  reqID <- param "id" :: ActionM Int
  case lookupStoryByID reqID allExampleStories of
    Just stry -> json stry
    Nothing   -> status notFound404
