{-# LANGUAGE DeriveAnyClass #-}
module Core.Types.GeoPosition where
import Data.Aeson
import GHC.Generics (Generic)
import Data.OpenApi (ToSchema)

data GeoPosition = GeoPosition
    {   longitude :: Double, 
        latitude :: Double
    } deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ValidateGeoPositionError = BadLongitude 
    | BadLatitude

validateGeoPosition :: GeoPosition -> Either ValidateGeoPositionError GeoPosition
validateGeoPosition geoPos@GeoPosition {..} = 
    if 
        |  (- 180) > longitude || longitude > 180 -> Left BadLongitude
        |  (- 90) > latitude || latitude > 90 -> Left BadLatitude
        | otherwise -> Right geoPos
