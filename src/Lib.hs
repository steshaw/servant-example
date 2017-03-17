{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Data.Time as Time

data User = User
  { userId           :: Int
  , userFirstName    :: String
  , userLastName     :: String
  , email            :: String
--  , registrationDate :: Time.UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type GetUsers =
  "users" :> Get '[JSON] [User]
  -- "GET /users" -> json user.

type GetUser =
  "user" :> Capture "userid" Integer :> Get '[JSON] User
  -- "GET /user/:userid" -> json user.

type API = GetUsers -- :<|> GetUser

-- "users"
--  :> QueryParam "sortby" SortBy
--    :> Get '[JSON] [User]
--  :<|> AdminsAPI

type AdminsAPI =
  "admins" :> Get '[JSON] [User]
  -- "GET /admins/", -> json list of users.

type UserAPI =
  "user" :> Capture "userid" Integer :> Get '[JSON] User
  -- "GET /user/:userid", -> json user.

type PostUser =
  "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  -- "POST /users", json user -> json user.

type PutUser =
  "users" :> Capture "userid" Integer
          :> ReqBody '[JSON] User
          :> Put '[JSON] User
  -- "PUT /users/:userid" with json user -> json user.

type UserAPI8 =
  "users"
    :> Header "User-Agent" Text
    :> Get '[JSON] [User]

type UserAPI9 =
  "users"
    :> Header "Accept" Text
    :> Get '[JSON] [User]

type UserAPI2 =
  "users"
    :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

type ProtectedAPI
    =  UserAPI                               -- this is public
  :<|> BasicAuth "my-realm" User :> UserAPI2 -- this is protected by auth

type UserAPI11 =
       "users" :> Get '[JSON] [User] -- "/users"
  :<|> Raw
   -- Requests to anything else than "/users"
   -- go here, where the server will try to
   -- find a file with the right name
   -- at the right path.

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
