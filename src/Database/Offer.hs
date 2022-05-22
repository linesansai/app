{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Offer where
import Database.Persist.TH
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto.Legacy
import Data.Hashable (hash)
import Data.Time (getCurrentTime)
import UnliftIO (liftIO)
import Database.User (UserId)
import Core.Types.GeoPosition (GeoPosition, latitude, longitude)
import Database.Esqueleto.Internal.Internal ( Update )
import GHC.Generics (Generic)
import Data.Aeson
import Data.OpenApi (ToSchema, ToParamSchema)

data OfferStatus = Open | Executing | Done | Closed
    deriving (Generic, Eq, Ord, Show, Read, FromJSON, ToJSON, Enum, Bounded, ToSchema)

allOfferStatuses :: [OfferStatus]
allOfferStatuses = [minBound .. maxBound]

derivePersistField "OfferStatus"

share
  [mkPersist sqlSettings, mkMigrate "migrateOffer"]
  [persistLowerCase|
Offer 
    createdAt Time.UTCTime
    status OfferStatus
    title T.Text
    description T.Text
    latitude Double
    longitude Double
    userId UserId
    executor UserId Maybe
    confirmedByCustomer Bool
    confirmedByExecutor Bool
    reward Int
    deriving Show Eq
  |]

deriving via Int instance ToSchema OfferId 
deriving via Int instance ToParamSchema OfferId 


data CreateOffer = CreateOffer
  { userId :: UserId,
    title :: T.Text,
    description :: T.Text,
    reward :: Int,
    geoPosition :: GeoPosition
  }

createOffer :: CreateOffer -> SqlPersistM (Entity Offer)
createOffer CreateOffer {..} = do 
  now <- liftIO getCurrentTime
  let record = Offer 
        {
          offerCreatedAt = now,
          offerStatus = Open,
          offerTitle = title,
          offerDescription = description,
          offerLatitude = latitude geoPosition,
          offerLongitude = longitude geoPosition,
          offerUserId = userId,
          offerExecutor = Nothing,
          offerConfirmedByCustomer = False,
          offerConfirmedByExecutor = False,
          offerReward = reward
        }
  key <- insert record 
  pure $ Entity key record

updateOffer :: 
    OfferId
    -> [SqlExpr (Entity Offer) -> SqlExpr Update]
    -> SqlPersistM  ()
updateOffer offerId updates = update $ \ca -> do
    set ca updates
    where_ $ ca ^. OfferId ==. val offerId

loadOfferById :: OfferId -> SqlPersistM (Maybe (Entity Offer))
loadOfferById offerId = fmap (Entity offerId) <$> get offerId

loadOffersInStatus :: [OfferStatus] -> SqlPersistM [Entity Offer]
loadOffersInStatus statuses = select $ from \row -> do 
    where_ $ row ^. #status `in_` valList statuses
    pure row

loadOffersOfUser :: UserId -> SqlPersistM [Entity Offer]
loadOffersOfUser userId = select $ from \row -> do 
    where_ $ row ^. #userId ==. val userId
    pure row

loadOffersTakenByUser :: UserId -> SqlPersistM [Entity Offer]
loadOffersTakenByUser userId = select $ from \row -> do 
    where_ $ row ^. #executor ==. just (val userId)
    pure row
