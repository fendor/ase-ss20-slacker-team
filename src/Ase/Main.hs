module Ase.Main where

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant.Api
import           Database.PostgreSQL.Simple
import           Data.Function

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  withStdoutLogger $ \logger -> do
    let settings = defaultSettings & setPort 8081 & setLogger logger
    putStrLn "Server starting on port 8081..."
    runSettings settings (aseApp conn)
