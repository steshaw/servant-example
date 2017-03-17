{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
-- import Data.Aeson.TH
import Data.Monoid
import qualified Data.Time.Calendar as Calendar
import Data.Text
import qualified Data.Time as Time
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId           :: Int
  , userFirstName    :: String
  , userLastName     :: String
  , email            :: String
  , registrationDate :: Time.Day
  } deriving (Eq, Show, Generic)

instance ToJSON User
-- $(deriveJSON defaultOptions ''User)

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

type GetUsers =
  "users" :> Get '[JSON] [User]
  -- "GET /users" -> json user.

type GetUser =
  "user" :> Capture "userid" Integer :> Get '[JSON] User
  -- "GET /user/:userid" -> json user.

type API
    =  GetUsers
  :<|> "albert" :> Get '[JSON] User
  :<|> "issac" :> Get '[JSON] User

issac = User 1 "Isaac" "Newton" "issac.newton@gmail.com" (Calendar.fromGregorian 1683 3 1)
albert = User 2 "Albert" "Einstein" "albert.einstein@gmail.com" (Calendar.fromGregorian 1905 12 1)

users :: [User]
users = [issac, albert]

server :: Server API
server = return users
    :<|> return albert
    :<|> return issac

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

port = 8080

startApp :: IO ()
startApp = do
  putStrLn $ "Listening on " <> show port
  run port app
