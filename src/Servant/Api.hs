module Servant.Api where

import           Data.Aeson
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Types

type HorseApi =
       "horse" :> Get '[JSON] [Model Horse]
  :<|> "horse" :> ReqBody '[JSON] Horse :> Post '[JSON] (Model Horse)
  :<|> "horse" :> Capture "horseId" Int :> ReqBody '[JSON] Horse :> Patch '[JSON] ()
  :<|> "horse" :> Capture "horseId" Int :> Delete '[JSON] ()
  :<|> "horse" :> Capture "horseId" Int :> Get '[JSON] (Model Horse)
