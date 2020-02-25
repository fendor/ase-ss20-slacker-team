module AseMain where

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Api
import           Database.PostgreSQL.Simple
import           Data.Function
import           UnliftIO.Exception

main :: IO ()
main =
  bracket (connect defaultConnectInfo { connectUser = "baldr" }) close
    $ \conn -> withStdoutLogger $ \logger -> do
        let settings = defaultSettings & setPort 8081 & setLogger logger
        putStrLn "Server starting on port 8081..."
        runSettings settings (aseApp conn)
