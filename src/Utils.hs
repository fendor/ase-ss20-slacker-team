{-# LANGUAGE TemplateHaskell #-}
module Utils where

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



data Model a = Model
    { modelId :: Int
    , model :: a
    }
    deriving (Show, Eq, Read, Generic, Typeable)

modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . Aeson.camelTo2 '_'

prefixOptions :: Aeson.Options
prefixOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = modifier }

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
      .~ (fromList [("id", intSchema)] <> (modelSchema ^. schema . properties))

instance Arbitrary a => Arbitrary (Model a) where
  arbitrary = genericArbitrary uniform
