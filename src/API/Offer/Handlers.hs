module API.Offer.Handlers where

import API.Offer.Types
import API.Response (WebResponse, emptySuccess, failWith, failWith'Plain, successWith)
import Control.Monad (when)
import Control.Monad.Except
import Core.AppMonad (MonadApp, runDB)
import Core.Auth (genJWTToken, withUserAuth)
import Core.Types.Auth
import Core.Types.GeoPosition (GeoPosition (..), validateGeoPosition)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Database.Esqueleto.Internal.Internal (Update)
import Database.Esqueleto.Legacy (Entity (entityVal), SqlExpr, val, (=.))
import qualified Database.Offer as DB
import Database.Persist (Entity (Entity))
import Database.User (UserId)
import qualified Database.User as DB
import Ext.Persistent (intToKey)
import Servant (ServerT, type (:<|>) (..))
import Servant.Auth.Server (AuthResult)

handlers :: MonadApp m => ServerT OfferAPI m
handlers =
  (getOwned :<|> getTaken)
    :<|> getOpen
    :<|> createOffer
    :<|> takeOffer
    :<|> confirmOffer
    :<|> cancelOffer
    :<|> clearExecutorOffer

getOwned :: MonadApp m => AuthResult AuthenticatedUser -> m (WebResponse () [Offer])
getOwned auth = withUserAuth auth \userId -> do
  offers <- runDB (DB.loadOffersOfUser userId)
  pure $ successWith $ map transformOffer offers

getTaken :: MonadApp m => AuthResult AuthenticatedUser -> m (WebResponse () [Offer])
getTaken auth = withUserAuth auth \userId -> do
  offers <- runDB (DB.loadOffersTakenByUser userId)
  pure $ successWith $ map transformOffer offers

getOpen :: MonadApp m => AuthResult AuthenticatedUser -> m (WebResponse () [Offer])
getOpen auth = withUserAuth auth \userId -> do
  offers <- runDB (DB.loadOffersInStatus [DB.Open])
  pure $ successWith $ map transformOffer offers

createOffer :: MonadApp m => AuthResult AuthenticatedUser -> CreateOfferRequest -> m (WebResponse CreateOfferError Offer)
createOffer auth CreateOfferRequest {..} = withConfirmedEmail auth \userId -> do
  offers <- runDB (DB.loadOffersOfUser userId)
  let isOfferOpen = (DB.Open ==) . DB.offerStatus . entityVal
      totalRewards = sum $ map (DB.offerReward . entityVal) $ filter isOfferOpen offers
      minimumRequiredBalance = totalRewards + reward
  runDB (DB.loadUserById userId) >>= \case
    Just (Entity _ DB.User {..}) | userBalance < minimumRequiredBalance -> pure $ failWith BalanceIsNotEnough
    Just (Entity _ DB.User {..}) | isLeft (validateGeoPosition geoPosition) -> pure $ failWith BadGeoPosition
    _ -> do
      let createReq =
            DB.CreateOffer
              { userId,
                title,
                description,
                geoPosition,
                reward
              }
      r <- runDB $ DB.createOffer createReq
      pure $ successWith $ transformOffer r

takeOffer :: MonadApp m => DB.OfferId -> AuthResult AuthenticatedUser -> m (WebResponse TakeOfferError ())
takeOffer offerId auth = withConfirmedEmail auth \userId -> do
  runDB (DB.loadOfferById offerId) >>= \case
    Nothing -> pure $ failWith OfferNotFound
    Just (Entity _ DB.Offer {..})
      | userId == offerUserId -> pure $ failWith YouCanNotTakeOwnOffer
      | isNothing offerExecutor && offerStatus == DB.Open -> do
        runDB (DB.updateOffer offerId [#executor =. val (Just userId), #status =. val DB.Executing])
        pure emptySuccess
    _ -> pure $ failWith OfferIsAlreadyTaken

confirmOffer :: MonadApp m => DB.OfferId -> AuthResult AuthenticatedUser -> m (WebResponse TakeOfferError ())
confirmOffer offerId auth = withConfirmedEmail auth \userId -> do
  runDB (DB.loadOfferById offerId) >>= \case
    Nothing -> pure $ failWith OfferNotFound
    Just (Entity _ offer@DB.Offer {..}) | offerUserId == userId && offerStatus == DB.Executing -> runDB do
      executor <- join <$> DB.loadUserById `traverse` offerExecutor
      DB.updateOffer offerId [#confirmedByCustomer =. val True, #status =. val DB.Done]
      updateBalances offer
      pure emptySuccess
    _ -> pure $ failWith OfferNotFound
  where
    updateBalances DB.Offer {..} = do
      mbCustomer <- DB.loadUserById offerUserId
      mbExecutor <- join <$> DB.loadUserById `traverse` offerExecutor
      case (mbCustomer, mbExecutor) of
        (Just (Entity customerId customer), Just (Entity executorId executor)) -> do
          let newCustomerBalance = DB.userBalance customer - offerReward
              newExecutorBalance = DB.userBalance executor + offerReward
          DB.updateUser customerId [#balance =. val newCustomerBalance]
          DB.updateUser executorId [#balance =. val newExecutorBalance]
        _ -> pure ()

clearExecutorOffer :: MonadApp m => DB.OfferId -> AuthResult AuthenticatedUser -> m (WebResponse TakeOfferError ())
clearExecutorOffer offerId auth = withConfirmedEmail auth \userId -> do
  runDB (DB.loadOfferById offerId) >>= \case
    Nothing -> pure $ failWith OfferNotFound
    Just (Entity _ DB.Offer {..})
      | offerUserId == userId || offerExecutor == Just userId -> do
        runDB (DB.updateOffer offerId [#executor =. val Nothing, #status =. val DB.Open])
        pure emptySuccess
    _ -> pure $ failWith OfferNotFound

cancelOffer :: MonadApp m => DB.OfferId -> AuthResult AuthenticatedUser -> m (WebResponse TakeOfferError ())
cancelOffer offerId auth = withConfirmedEmail auth \userId -> do
  runDB (DB.loadOfferById offerId) >>= \case
    Nothing -> pure $ failWith OfferNotFound
    Just (Entity _ offer@DB.Offer {..}) | offerUserId == userId -> runDB do
      DB.updateOffer offerId [#status =. val DB.Closed]
      pure emptySuccess
    _ -> pure $ failWith OfferNotFound

transformOffer :: Entity DB.Offer -> Offer
transformOffer (Entity offerId DB.Offer {..}) =
  Offer
    { offerId = offerId,
      title = offerTitle,
      description = offerDescription,
      userId = offerUserId,
      geoPosition = GeoPosition {longitude = offerLongitude, latitude = offerLatitude},
      status = offerStatus,
      reward = offerReward,
      executor = offerExecutor
    }

withConfirmedEmail :: MonadApp m => AuthResult AuthenticatedUser -> (UserId -> m (WebResponse e a)) -> m (WebResponse e a)
withConfirmedEmail auth action = withUserAuth auth \userId ->
  runDB (DB.loadUserById userId) >>= \case
    Just user | isEmailConfirmed user -> action userId
    _ -> pure $ failWith'Plain ("EmailIsNotVerified", "You need to confirm your email first")
  where
    isEmailConfirmed = DB.userIsEmailConfirmed . entityVal