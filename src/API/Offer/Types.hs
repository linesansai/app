{-# LANGUAGE DeriveAnyClass #-}

module API.Offer.Types where

import API.Response
import Core.Auth
import Core.Types.Auth
import Core.Types.GeoPosition (GeoPosition)
import Core.Utils (tshow)
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Database.Offer (OfferId, OfferStatus)
import Database.User (UserId)
import Servant.API
import Servant.API.Generic

type OfferAPI =
  "offers"
    :> ( GetMyOffers
           :<|> GetOpenOffers
           :<|> CreateOffer
           :<|> TakeOffer
           :<|> ConfirmOffer
           :<|> CancelOffer
           :<|> ClearExecutorOffer
       )

type GetMyOffers =
  "my"
    :> (GetOwnedOffers :<|> GetTakenOffers)

type GetOwnedOffers =
  "owned"
    :> ProtectedWithJWT
    :> Get '[JSON] (WebResponse () [Offer])

type GetTakenOffers =
  "taken"
    :> ProtectedWithJWT
    :> Get '[JSON] (WebResponse () [Offer])

type GetOpenOffers =
  ProtectedWithJWT
    :> Get '[JSON] (WebResponse () [Offer])

data Offer = Offer
  { offerId :: OfferId,
    title :: T.Text,
    description :: T.Text,
    userId :: UserId,
    geoPosition :: GeoPosition,
    status :: OfferStatus,
    reward :: Int,
    executor :: Maybe UserId
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

type CreateOffer =
  ProtectedWithJWT
    :> ReqBody '[JSON] CreateOfferRequest
    :> Post '[JSON] (WebResponse CreateOfferError Offer)

data CreateOfferRequest = CreateOfferRequest
  { title :: T.Text,
    description :: T.Text,
    geoPosition :: GeoPosition,
    reward :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data CreateOfferError
  = BadGeoPosition
  | BalanceIsNotEnough
  deriving (Eq, Show)

instance IsAPIError CreateOfferError where
  toAPIError = \case
    BadGeoPosition -> ("BadGeoPosition", "")
    BalanceIsNotEnough -> ("BalanceIsNotEnough", "")

type TakeOffer =
  "take" :> Capture "offerId" OfferId :> ProtectedWithJWT
    :> Put '[JSON] (WebResponse TakeOfferError ())

data TakeOfferError = OfferNotFound | OfferIsAlreadyTaken | YouCanNotTakeOwnOffer
  deriving (Eq, Show)

instance IsAPIError TakeOfferError where
  toAPIError = \case
    OfferNotFound -> ("OfferNotFound", "")
    OfferIsAlreadyTaken -> ("OfferIsAlreadyTaken", "")
    YouCanNotTakeOwnOffer -> ("YouCanNotTakeOwnOffer", "")

type ConfirmOffer =
  "confirm" :> Capture "offerId" OfferId :> ProtectedWithJWT
    :> Put '[JSON] (WebResponse TakeOfferError ())

type ClearExecutorOffer =
  "clear-executor" :> Capture "offerId" OfferId :> ProtectedWithJWT
    :> Put '[JSON] (WebResponse TakeOfferError ())

type CancelOffer =
  "cancel" :> Capture "offerId" OfferId :> ProtectedWithJWT
    :> Put '[JSON] (WebResponse TakeOfferError ())