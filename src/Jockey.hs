module Jockey where

import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.Aeson.Types              as Aeson
import           GHC.Generics
import           Data.Swagger.Schema
import           Data.Swagger
import           Data.Proxy
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Reader
import           Polysemy.Error
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Control.Lens.Operators  hiding ( (.=) )
import           Data.HashMap.Strict.InsOrd     ( fromList )
import           Data.Maybe
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text ( )
import           Generic.Random
import           Type.Reflection
import           Utils
import           Postgres.Polysemy              ( DbCrud(..) )
import qualified Postgres.Polysemy             as P
import           Servant

data Jockey = Jockey
    { jockeyName :: T.Text
    , jockeySkill :: Double
    , jockeyAge :: Int
    , jockeyDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, Typeable)

type JockeyApi = "jockey" :>
  (    Summary "Get all entities"   :> Get '[JSON] [Model Jockey]
  :<|> Summary "Create new entity"  :> ReqBody '[JSON] Jockey :> Post '[JSON] (Model Jockey)
  :<|> Summary "Update entity"      :> Capture "jockeyId" Int :> ReqBody '[JSON] Jockey :> Patch '[JSON] ()
  :<|> Summary "Delete entity"      :> Capture "jockeyId" Int :> Delete '[JSON] ()
  :<|> Summary "Find entity by id"  :> Capture "jockeyId" Int :> Get '[JSON] (Model Jockey)
  )

jockeyApi
  :: Members '[DbCrud Jockey, Error T.Text] r => ServerT JockeyApi (Sem r)
jockeyApi =
  P.findAll :<|> P.insert :<|> update' :<|> P.delete @Jockey :<|> findOne'
 where
  update' :: Member (DbCrud Jockey) r => Int -> Jockey -> Sem r ()
  update' hid jockey = P.update (Model hid jockey)

  findOne'
    :: Members '[DbCrud Jockey, Error T.Text] r => Int -> Sem r (Model Jockey)
  findOne' hid = P.findOne hid >>= \case
    Nothing -> throw @T.Text ""
    Just h  -> return h

instance FromRow Jockey
instance ToRow Jockey

instance FromJSON Jockey where
  parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON Jockey where
  toJSON     = Aeson.genericToJSON prefixOptions
  toEncoding = Aeson.genericToEncoding prefixOptions

instance Arbitrary Jockey where
  arbitrary = genericArbitrary uniform

instance ToSchema Jockey where
  declareNamedSchema _ = do
    textSchema   <- declareSchemaRef (Proxy :: Proxy T.Text)
    doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
    intSchema    <- declareSchemaRef (Proxy :: Proxy Int)
    boolSchema   <- declareSchemaRef (Proxy :: Proxy Bool)
    return
      $  NamedSchema (Just "Jockey")
      $  mempty
      &  type_
      ?~ SwaggerObject
      &  description
      ?~ "Jockey Entity"
      &  properties
      .~ fromList
           [ ("name"   , textSchema)
           , ("skill"  , doubleSchema)
           , ("age"    , intSchema)
           , ("deleted", boolSchema)
           ]

runPostgresJockey
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Jockey : r) a
  -> Sem r a
runPostgresJockey = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert Jockey {..} -> do
      [Only jid] <- embed $ returning
        conn
        [sql|INSERT INTO JOCKEY (name, skill, age)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(jockeyName, jockeySkill, jockeyAge)]
      return (Model jid Jockey { .. })
    FindOne hid -> do
      res <- embed $ query
        conn
        [sql|SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?
        |]
        (Only hid)
      return (listToMaybe res)
    FindAll -> embed $ query_
      conn
      "SELECT id, name, skill, age, deleted FROM JOCKEY WHERE deleted = FALSE"

    Update (Model jid Jockey {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
        (jockeyName, jockeySkill, jockeyAge, jid)
      return ()
    Delete jid -> do
      _ <- embed $ execute conn
                           "UPDATE Jockey SET deleted = True WHERE id = ?"
                           (Only jid)
      return ()
