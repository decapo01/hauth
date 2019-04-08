module Adapter.InMemory.Auth where


import ClassyPrelude

import qualified Domain.Auth as D

import Data.Has
import Text.StringRandom
import Control.Monad.Except


data State = State {

  stateAuths            :: [(D.UserId,D.Auth)],
  stateUnverifiedEmails :: Map D.VerificationCode D.Email,
  stateVerifiedEmails   :: Set D.Email,
  stateUserIdCounter    :: Int,
  stateNotifications    :: Map D.Email D.VerificationCode,
  stateSessions         :: Map D.SessionId D.UserId

} deriving (Show,Eq)



initialState :: State
initialState = State {
  stateAuths            = [],
  stateUnverifiedEmails = mempty,
  stateVerifiedEmails   = mempty,
  stateUserIdCounter    = 0,
  stateNotifications    = mempty,
  stateSessions         = mempty
}

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

-- My Implementation
-- addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
-- addAuth auth = do
--   vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
--   tvar <- asks getter
--   atomically $ do
--     state <- readTVar tvar
--     let maybeVerified = find(\a -> (D.authEmail $ snd a) == D.authEmail auth ) $ stateAuths state
--     case maybeVerified of
--       Just email -> return $ Left D.RegistrationErrorEmailTaken
--       Nothing    -> do
--         let userCount      = stateUserIdCounter    state
--             unverifieds    = stateUnverifiedEmails state
--             newCount       = userCount + 1
--             email          = D.authEmail auth
--             newUnverifieds = insertMap ((tshow newCount) <> vCode) email unverifieds
--             newState       = state { stateUnverifiedEmails = newUnverifieds, stateUserIdCounter = newCount }
--         writeTVar tvar newState
--         return $ Right vCode


-- Books Implementation
addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth auth = do
  tvar  <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let auths       = stateAuths state
        email       = D.authEmail auth
        isDuplicate = any (email ==) . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    let newUserId      = stateUserIdCounter state + 1
        newAuths       = (newUserId, auth) : auths
        unverifieds    = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState       = state
          { stateAuths            = newAuths
          , stateUserIdCounter    = newUserId
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return vCode


-- Did this alternate of the function just to understand
setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified vCode = do
  tvar  <- asks getter
  atomically $ do
    state <- readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        verifieds   = stateVerifiedEmails   state
        mayEmail    = lookup vCode unverifieds
    case mayEmail of
      Nothing    -> return $ Left D.EmailVerificationErrorInvalidCode
      Just email -> do
        let newUnverifieds = deleteMap vCode unverifieds
            newVerifieds   = insertSet email verifieds
            newState       = state { stateUnverifiedEmails = newUnverifieds, stateVerifiedEmails = newVerifieds}
        writeTVar tvar newState
        return $ Right ()

-- Original in book
-- setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError ())
-- setEmailAsVerified vCode = do
--   tvar  <- asks getter
--   atomically . runExceptT $ do
--     state <- lift $ readTVar tvar
--     let unverifieds = stateUnverifiedEmails state
--         verifieds   = stateVerifiedEmails   state
--         mayEmail    = lookup vCode unverifieds
--     case mayEmail of
--       Nothing    -> throwError D.EmailVerificationErrorInvalidCode
--       Just email -> do
--         let newUnverifieds = deleteMap vCode unverifieds
--             newVerifieds   = insertSet email verifieds
--             newState       = state { stateUnverifiedEmails = newUnverifieds, stateVerifiedEmails = newVerifieds}
--         lift $ writeTVar tvar newState

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeAuth = find(\a -> (D.authEmail $ snd a) == D.authEmail auth) $ stateAuths state
  case maybeAuth of
    Nothing  -> return Nothing
    Just authTuple -> do
      return $ Just (fst authTuple,True)



findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeAuth = map snd $ find(\auth -> fst auth == uId) $ stateAuths state
  return $ D.authEmail <$> maybeAuth


notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications    = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState         = state { stateNotifications = newNotifications }
    writeTVar tvar newState

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId  <- liftIO $ ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions    = stateSessions state
        newSessions = insertMap sId uId sessions
        newState    = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar


getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state
