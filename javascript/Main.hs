{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import GHC.Generics
import qualified Language.Javascript.JQuery as JQuery
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Servant
import Servant.JS
-- import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import System.Random

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point

data Search a = Search
  { query :: Text
  , results :: [a]
  } deriving Generic

instance ToJSON a => ToJSON (Search a)

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

data Book = Book
  { author :: Text
  , title :: Text
  , year :: Int
  } deriving Generic

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book

type API
  = "point" :> Get '[JSON] Point
  :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]

searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing  = return (mkSearch "" books)
searchBook (Just q) = return (mkSearch q books')
  where
    books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                        || q' `T.isInfixOf` T.toLower (title b)
                    )
                    books
    q' = T.toLower q

randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Point rx ry, g'')

api :: Proxy API
api = Proxy

server :: Server API
server = randomPoint
    :<|> searchBook

type API' = API :<|> Raw

api' :: Proxy API'
api' = Proxy

server' :: Server API'
server' = server
     -- :<|> serveDirectoryFileServer "static"
     :<|> serveDirectory "static"

app :: Application
app = serve api' server'

apiJS1 :: Text
apiJS1 = jsForAPI api jquery

writeJsFiles :: IO ()
writeJsFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- T.readFile =<< JQuery.file
  T.writeFile "static/jq.js" jq

port = 8080

main :: IO ()
main = do
  writeJsFiles
  putStrLn $ "jQuery version " ++ show JQuery.version
  putStrLn $ "jQuery url " ++ show JQuery.url
  Warp.run port app
  putStrLn $ "Listening on " ++ show port
