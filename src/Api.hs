module Api where

import           Servant
import           Servant.Swagger.UI
import           Servant.Swagger
import           Control.Lens.Operators
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Swagger.Lens
import           Data.Swagger
import           Horse
import           Jockey
import           Polysemy.Internal
import           Polysemy.Reader
import           Polysemy.Fail
import           Polysemy.Error
import           Postgres.Polysemy              ( DbCrud(..) )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple

type AseApi =
         HorseApi
    :<|> JockeyApi

type Api = AseApi
    -- :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

api
  :: Members '[DbCrud Horse, DbCrud Jockey, Error T.Text] r
  => ServerT Api (Sem r)
api = (horseApi :<|> jockeyApi)
  -- :<|> swaggerServe

 where
  -- swaggerServe :: ServerT (SwaggerSchemaUI' "swagger-ui" "swagger.json") (Sem r)
  -- swaggerServe = swaggerSchemaUIServer swaggerDoc
  swaggerDoc :: Swagger
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

type AppEffects
  = DbCrud Jockey : DbCrud Horse : Error T.Text : Fail : Reader Connection : Embed IO : '[]

toHandler :: Connection -> Sem AppEffects a -> Handler a
toHandler conn app = do
  res <- liftIO $ runM
    ( runReader conn
    . runFail
    . runError
    . runPostgresHorse
    . runPostgresJockey
    $ app
    )
  case res of
    Left  _err -> throwError err500
    Right r    -> case r of
      Left  _err -> throwError err404
      Right s    -> return s


aseApp :: Connection -> Application
aseApp conn = serve (Proxy :: Proxy Api)
                    (hoistServer (Proxy :: Proxy Api) (toHandler conn) api)
