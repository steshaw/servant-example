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
import Data.List as List
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

type GetUser =
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

type UserAPI
    =  GetUsers
  :<|> "albert" :> Get '[JSON] User
  :<|> "issac" :> Get '[JSON] User

issac = User 1 "Isaac" "Newton" "issac.newton@gmail.com" (Calendar.fromGregorian 1683 3 1)
albert = User 2 "Albert" "Einstein" "albert.einstein@gmail.com" (Calendar.fromGregorian 1905 12 1)

users :: [User]
users = [issac, albert]

userServer :: Server UserAPI
userServer
     = return users
  :<|> return albert
  :<|> return issac

data Position = Position
  { xCoord :: Int
  , yCoord:: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

type API3
    =  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> (Post '[JSON] Email)

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    name = clientName c
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " <> name <> ", we miss you!"
    body' = "Hi " <> name <> ",\n\n"
         <> "Since you've recently turned " <> show (clientAge c)
         <> ", have you checked out our latest "
         <> List.intercalate ", " (clientInterestedIn c)
         <> " products? Give us a visit!"

server3 :: Server API3
server3
    =    position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello optName = return $ HelloMessage $ case optName of
      Nothing -> "Hello, anonymous coward"
      Just name -> "Hello, " <> name

    marketing :: ClientInfo -> Handler Email
    marketing clientInfo = return (emailForClient clientInfo)

type API = UserAPI :<|> API3

api :: Proxy API
api = Proxy

server :: Server API
server = userServer :<|> server3

app :: Application
app = serve api server

port = 8080

startApp :: IO ()
startApp = do
  putStrLn $ "Listening on " <> show port
  run port app
