{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Routes
( routes
) where

import Control.Monad.IO.Class

import Web.Scotty
import Network.HTTP.Types (notFound404)

import Story (_meta, allExampleStories, lookupStoryByID)
import DB (withDB, selectStoryMetas, selectStory)

---------------------------

routes = do
  get "/stories"   stories
  get "/story/:id" story

---------------------------

stories :: ActionM ()
stories = liftIO (withDB selectStoryMetas) >>= json

story :: ActionM ()
story = param "id" >>= (liftIO . withDB . selectStory)
                   >>= maybe (status notFound404) json
