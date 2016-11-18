{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Except   (throwE)
import Data.Aeson                   (ToJSON, FromJSON)
import Data.List                    (find)
import GHC.Generics                 (Generic)
import Network.Wai                  (responseFile)
import Network.HTTP.Types           (hContentType, status200)
import Network.Wai.Handler.Warp     (run)
import Servant

data Wombat = Wombat
  { id :: Int
  , name :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Wombat

wombatStore :: [Wombat]
wombatStore = 
  [ Wombat 0 "Gertrude"
  , Wombat 1 "Horace"
  , Wombat 2 "Maisie"
  , Wombat 3 "Julius"
  ]

wombats :: Handler [Wombat]
wombats = return wombatStore

wombat :: Int -> Handler Wombat
wombat wid = do
  case find (\w -> Main.id w == wid) wombatStore of
    Just x -> return x
    Nothing -> throwE err404

type API = 
    "api" :> "wombats" :> Get '[JSON] [Wombat] :<|>
    "api" :> "wombats" :> Capture "id" Int :> Get '[JSON] Wombat :<|>
    Raw

api :: Proxy API
api = Proxy

server :: Server API 
server = wombats
  :<|> wombat
  :<|> staticOrDefault

staticOrDefault :: Application
staticOrDefault req respond = respond $ 
  responseFile
  status200
  [(hContentType, "text/html")]
  "static/index.html"
  Nothing

app :: Application
app = serve api server

main :: IO ()
main = run 3000 app
