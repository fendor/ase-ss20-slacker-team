module Types where

import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.Aeson.Types              as Aeson
import           GHC.Generics
import           Data.Swagger.Schema
import           Data.Swagger
import           Data.Proxy
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Control.Lens.Operators  hiding ( (.=) )
import           Data.HashMap.Strict.InsOrd     ( fromList )
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text ( )
import           Generic.Random
import           Type.Reflection

data Horse = Horse
    { horseName :: T.Text
    , horseSpeed :: Double
    , horseImage :: T.Text
    , horseDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, Typeable)

data Jockey = Jockey
    { jockeyName :: T.Text
    , jockeySkill :: Double
    , jockeyAge :: Int
    , jockeyDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, Typeable)

data RaceEntry = RaceEntry
    { raceJockey :: Model Jockey
    , raceHorse :: Model Horse
    , raceId :: Int
    }
    deriving (Show, Eq, Read, Generic, Typeable)

data Model a = Model
    { modelId :: Int
    , model :: a
    }
    deriving (Show, Eq, Read, Generic, Typeable)

instance FromRow Jockey
instance FromRow Horse
instance ToRow Jockey
instance ToRow Horse

modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . Aeson.camelTo2 '_'

prefixOptions :: Aeson.Options
prefixOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = modifier }

instance FromJSON Horse where
    parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON Horse where
    toJSON     = Aeson.genericToJSON prefixOptions
    toEncoding = Aeson.genericToEncoding prefixOptions

instance FromJSON Jockey where
    parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON Jockey where
    toJSON     = Aeson.genericToJSON prefixOptions
    toEncoding = Aeson.genericToEncoding prefixOptions

instance FromJSON RaceEntry where
    parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON RaceEntry where
    toJSON     = Aeson.genericToJSON prefixOptions
    toEncoding = Aeson.genericToEncoding prefixOptions

instance FromJSON a => FromJSON (Model a) where
    parseJSON = withObject "Model" $ \val -> do
        mid     <- val .: "id"
        content <- parseJSON (Object val)
        return (Model mid content)


instance ToJSON a => ToJSON (Model a) where
    toJSON (Model mid content) =
        let Aeson.Object l = object ["id" .= mid]
            Aeson.Object r = toJSON content
        in  Aeson.Object (l <> r)

    -- toEncoding = genericToEncoding defaultOptions

instance FromRow a => FromRow (Model a) where
    fromRow = Model <$> field <*> fromRow

instance FromRow RaceEntry where
    fromRow = RaceEntry <$> fromRow <*> fromRow <*> field

instance ToSchema a => ToSchema (Model a) where
    declareNamedSchema _ = do
        intSchema   <- declareSchemaRef (Proxy :: Proxy Int)
        modelSchema <- declareNamedSchema (Proxy :: Proxy a)
        return
            $  NamedSchema (Just "Model")
            $  mempty
            &  type_
            ?~ SwaggerObject
            &  properties
            .~ (  fromList [("id", intSchema)]
               <> (modelSchema ^. schema . properties)
               )


instance ToSchema Horse where
    declareNamedSchema _ = do
        textSchema   <- declareSchemaRef (Proxy :: Proxy T.Text)
        doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
        boolSchema   <- declareSchemaRef (Proxy :: Proxy Bool)
        return
            $  NamedSchema (Just "Horse")
            $  mempty
            &  type_
            ?~ SwaggerObject
            -- &  description .~ Just "Test"
            &  properties
            .~ fromList
                   [ ("name"   , textSchema)
                   , ("speed"  , doubleSchema)
                   , ("image"  , textSchema)
                   , ("deleted", boolSchema)
                   ]


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
            &  properties
            .~ fromList
                   [ ("name"   , textSchema)
                   , ("skill"  , doubleSchema)
                   , ("age"    , intSchema)
                   , ("deleted", boolSchema)
                   ]

instance Arbitrary Horse where
    arbitrary = genericArbitrary uniform
instance Arbitrary Jockey where
    arbitrary = genericArbitrary uniform
instance Arbitrary RaceEntry where
    arbitrary = genericArbitrary uniform
instance Arbitrary a => Arbitrary (Model a) where
    arbitrary = genericArbitrary uniform
