module Main
  ( main
  )
where

import           Test.Hspec
import           Servant.Swagger
import           Data.Proxy
import           Servant.Api
import qualified Postgres.DatabaseSpec         as P

main :: IO ()
main = hspec $ do
  describe "database-tests" P.databaseSpec
  describe "swagger-ui" $ validateEveryToJSON (Proxy :: Proxy AseApi)
