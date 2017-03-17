{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userFirstName\":\"Isaac\",\"email\":\"issac.newton@gmail.com\",\"userLastName\":\"Newton\",\"registrationDate\":\"1683-03-01\",\"userId\":1},{\"userFirstName\":\"Albert\",\"email\":\"albert.einstein@gmail.com\",\"userLastName\":\"Einstein\",\"registrationDate\":\"1905-12-01\",\"userId\":2}]"
            get "/users" `shouldRespondWith` users
