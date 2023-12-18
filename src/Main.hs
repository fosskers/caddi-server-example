{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Main (main) where

import Data.Aeson (ToJSON)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Time (fromGregorian)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

type UserAPI = "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "name" String :> Get '[JSON] (Maybe User)

data User = User {
   name         :: !String
 , age          :: !Int
 , email        :: !String
 , registration :: !Day
} deriving (Eq, Show, Generic, ToJSON)

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

allUsers :: Handler [User]
allUsers = pure users

getUser :: String -> Handler (Maybe User)
getUser "" = pure Nothing
getUser q  = pure . listToMaybe $ mapMaybe g users
  where
    f :: String -> Maybe String
    f n = case words n of
      []  -> Nothing
      a:_ -> Just a

    g :: User -> Maybe User
    g u = case fmap (== q') . f $ name u of
      Just True  -> Just u
      Just False -> Nothing
      Nothing    -> Nothing

    q' = fromJust $ f q

server :: Server UserAPI
server = allUsers
  :<|> getUser

api :: Proxy UserAPI
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Listening on Port 8081."
  run 8081 app
