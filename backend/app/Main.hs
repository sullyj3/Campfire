module Main
( main
) where

import Network.Wai.Middleware.Cors
import Web.Scotty

import Routes (routes)

------------------------

main :: IO ()
main = scotty 5000 $ do
  middleware simpleCors
  routes

