{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Types.UUID where

import qualified Data.Text as T
import Data.Aeson
import Data.OpenApi (ToSchema, ToParamSchema)
import Servant (FromHttpApiData)

newtype UUID = UUID T.Text
  deriving newtype  (Eq, Ord, FromJSON, ToJSON, ToSchema, ToParamSchema, FromHttpApiData)