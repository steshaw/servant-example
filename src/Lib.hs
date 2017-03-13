{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.Time as Time

data User = User
  { userId           :: Int
  , userFirstName    :: String
  , userLastName     :: String
  , email            :: String
--  , registrationDate :: Time.UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users"
--  :> QueryParam "sortby" SortBy
  :> Get '[JSON] [User]

data SortBy = Age | Name

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users =
  [ User 1 "Isaac" "Newton" "issac.newton@gmail.com"
  , User 2 "Albert" "Einstein" "albert.einstein@gmail.com"
  ]
