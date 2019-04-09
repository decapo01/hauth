module Domain.Auth where


import ClassyPrelude

import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip


data Auth = Auth {
  authEmail    :: Email,
  authPassword :: Password

} deriving (Show,Eq)



data RegistrationError =
  RegistrationErrorEmailTaken
  deriving (Show, Eq)


newtype Email = Email {
  emailRaw :: Text

} deriving (Show,Eq,Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    "Not a valid Email"
  ]

newtype Password = Password {
  passwordRaw :: Text

} deriving (Show,Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50       "Should between 5 and 50"
  , regexMatches [re|\d|]    "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]


type VerificationCode = Text

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Show,Eq)

class (Monad m) => AuthRepo m where
  addAuth             :: Auth             -> m (Either RegistrationError (UserId,VerificationCode))
  setEmailAsVerified  :: VerificationCode -> m (Either EmailVerificationError (UserId,Email))
  findUserByAuth      :: Auth             -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId           -> m (Maybe Email)

class (Monad m) => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
  newSession            :: UserId    -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)


withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> "is registered successfully"


verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError (UserId,Email))
verifyEmail = setEmailAsVerified


type UserId = Int

type SessionId = Text

data LoginError =
    LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show,Eq)

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing        -> throwError LoginErrorInvalidAuth
    Just (_,False) -> throwError LoginErrorEmailNotVerified
    Just (uId,_)   -> withUserIdContext uId . lift $ do
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully "
      return sId


resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
