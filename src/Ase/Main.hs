module Ase.Main where

import Servant
import Network.Wai.Handler.Warp
import Servant.Api
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  run 8081 (aseApp conn)