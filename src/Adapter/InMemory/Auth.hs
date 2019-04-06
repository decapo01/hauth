module Adapter.InMemory.Auth where


import ClassyPrelude

import qualified Domain.Auth as D

import Data.Has

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

addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId = undefined

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification = undefined

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession = undefined

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId = undefined
