{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Routes
( routes
) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)

import Web.Scotty
import Network.HTTP.Types (notFound404)

import Story (_meta, allExampleStories, lookupStoryByID)
import DB (withDB, selectStoryMetas, selectStory)

---------------------------

routes :: ByteString -> ScottyM ()
routes dbAddr = do
  get "/stories"   stories dbAddr
  get "/story/:id" story   dbAddr

---------------------------

stories :: ByteString -> ActionM ()
stories dbAddr = liftIO (withDB dbAddr selectStoryMetas) >>= json

story :: ByteString -> ActionM ()
story dbAddr = param "id" >>= (liftIO . withDB dbAddr . selectStory)
                          >>= maybe (status notFound404) json
