{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp

import Lib (
  Email(..), ClientInfo(..), Position(..), HelloMessage(..),
  server3, emailForClient, API3)

instance ToCapture (Capture "x" Int) where
  toCapture _ =
    DocCapture "x"
      "(integer) position on the x axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ =
    DocCapture "y"
      "(integer) position on the y axis"

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"                     -- name
                  ["Alp", "John Doe", "..."] -- example of values (not necessarily exhaustive)
                  "Name of the person to say hello to." -- description
                  Normal -- Normal, List or Flag

instance ToSample Position where
  toSamples _ = singleSample (Position 3 14) -- example of output

instance ToSample HelloMessage where
toSamples _ =
  [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
  , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
  ]
  -- multiple examples to display this time

-- XXX: Why'd I have to add this instance?
instance ToSample Char where
  toSamples _ = [ ("Blah about 'a'", 'a')
                , ("Blah about 'b'", 'b')
                ]

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
  toSamples _ = singleSample ci

instance ToSample Email where
  toSamples _ = singleSample (emailForClient ci)

type API = API3 :<|> Raw

api :: Proxy Main.API
api = Proxy

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] api

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

server :: Server Main.API
server = Lib.server3 :<|> serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

app :: Application
app = serve api server

apiDocs :: Servant.Docs.API
apiDocs = docs api

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Listening on " ++ show port
  Warp.run port app
