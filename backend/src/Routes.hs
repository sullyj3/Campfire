{-# LANGUAGE OverloadedStrings #-}

module Routes
( routes
) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T

import Web.Scotty
import Network.HTTP.Types (notFound404, status501)

import DB (withDB, selectStoryMetas, selectStory, insertStory)
import Story (StoryUpload(..))

---------------------------

routes = do
  get "/stories"   stories
  get "/story/:id" story
  post "/upload"   upload

---------------------------

stories :: ActionM ()
stories = liftIO (withDB selectStoryMetas) >>= json

story :: ActionM ()
story = param "id" >>= getStoryID

getStoryID :: Int -> ActionM ()
getStoryID = maybe (status notFound404) json <=< (liftIO . withDB . selectStory)

upload :: ActionM ()
upload = do
  su <- jsonData :: ActionM StoryUpload
  liftIO $ withDB $ insertStory su
  text "inserted!"
