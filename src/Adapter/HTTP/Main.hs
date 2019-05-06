module Adapter.HTTP.Main where

import ClassyPrelude hiding (delete)

import Web.Scotty.Trans

main :: IO ()
main =
  scottyT 3000 id routes

routes :: (MonadIO m) => ScottyT LText m ()
routes =
  get "/hello" $ do
    name <- param "name" `rescue` \_ -> return "anonymous"
    text $ "Hello, " <> name