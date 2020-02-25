module Api where

import           Servant
import           Servant.Swagger.UI
import           Servant.Swagger
import           Database.PostgreSQL.Simple
import           Control.Lens.Operators
import           Data.Swagger.Lens
import           Horse
import           Jockey

type AseApi =
         HorseApi
    :<|> JockeyApi

type Api = AseApi
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Connection -> Server Api
api conn = (horseApi conn :<|> jockeyApi conn)
  :<|> swaggerSchemaUIServer swaggerDoc
 where
  swaggerDoc =
    (horseDoc <> jockeyDoc)
      &  info
      .  title
      .~ "Wendys Rennstall API"
      &  info
      .  version
      .~ "0.1"
      &  info
      .  description
      ?~ "This is an API for the Wendys Rennstall service"
      &  info
      .  license
      ?~ "MIT"
  horseDoc  = toSwagger (Proxy :: Proxy HorseApi)
  jockeyDoc = toSwagger (Proxy :: Proxy JockeyApi)

aseApp :: Connection -> Application
aseApp conn = serve (Proxy :: Proxy Api) (api conn)
