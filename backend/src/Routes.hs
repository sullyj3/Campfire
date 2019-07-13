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
  reqID <- param "id" :: ActionM Int
  case lookupStoryByID reqID allExampleStories of
    Just stry -> json stry
    Nothing   -> status notFound404
