{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Lib

instance FromJSON User
instance FromJSON Position
instance FromJSON HelloMessage
instance FromJSON Email

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ClientM Position

hello :: Maybe String -- ^ an optional value for "name"
      -> ClientM HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> ClientM Email

(getUsers :<|> albert :<|> issac)
  :<|> (position :<|> hello :<|> marketing)
  :<|> getPersons = client api

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

run :: ClientEnv -> IO ()
run env = do
  res <- runClientM queries env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

pokeUsers :: ClientM ()
pokeUsers = do
  users <- getUsers
  albert <- Main.albert
  issac <- Main.issac
  liftIO $ putStr "users = " >> print users
  liftIO $ putStr "albert = " >> print albert
  liftIO $ putStr "issac = " >> print issac

pokePersons :: ClientM ()
pokePersons = do
  persons <- getPersons
  liftIO $ putStr "persons = " >> print persons

main = do
  manager <- newManager defaultManagerSettings
  let env = (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  runClientM pokeUsers env
  runClientM pokePersons env
  run env
