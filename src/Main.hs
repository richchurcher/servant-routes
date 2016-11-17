{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.List
import GHC.Generics
import Network.Wai.Handler.Warp
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
  :<|> serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = run 3000 app
