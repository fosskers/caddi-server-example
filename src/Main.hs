{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Time (fromGregorian)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Control.Monad.IO.Class (MonadIO(liftIO))

type UserAPI = "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "name" String :> Get '[JSON] (Maybe User)
  :<|> "users" :> "new" :> ReqBody '[JSON] Submission :> Post '[PlainText] String

data Submission = Submission { subName :: !String, subAge :: !Int }
  deriving (Eq, Show, Generic, FromJSON)

data User = User {
   userName  :: !String
 , userAge   :: !Int
 , userEmail :: !String
 , userReg   :: !Day
} deriving (Eq, Show, Generic, ToJSON)

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  , User "Shunsuke Nakamura" 67 "ceo@caddi.jp"      (fromGregorian 1900 1 1)
  ]

allUsers :: Handler [User]
allUsers = pure users

-- TODO Refactor to make it pure.
-- TODO Yield a better error than `null` when a match isn't found.
getUser :: String -> Handler (Maybe User)
getUser "" = pure Nothing -- None
getUser q  = pure . listToMaybe $ mapMaybe g users -- filter_map
  where
    f :: String -> Maybe String
    f n = case words n of
      []  -> Nothing
      a:_ -> Just a

    g :: User -> Maybe User
    g u = case fmap (== q') . f $ userName u of
      Just True  -> Just u
      Just False -> Nothing
      Nothing    -> Nothing

    q' = fromJust $ f q

newSubmission :: Submission -> Handler String
newSubmission s = do
  liftIO $ putStrLn "Hi Ueno-san!"
  pure $ subName s

server :: Server UserAPI
server = allUsers
  :<|> getUser
  :<|> newSubmission

api :: Proxy UserAPI
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Listening on Port 8081."
  run 8081 app
