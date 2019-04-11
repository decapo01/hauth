module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Data.Has
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import qualified Domain.Auth                          as D
import           Text.StringRandom

type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config {
  configUrl                  :: ByteString,
  configStripeCount          :: Int,
  configMaxOpenConnPerStripe :: Int,
  configIdleConnTimeout      :: NominalDiffTime
}


migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
  where
    cmds = [
      MigrationInitialization,
      MigrationDirectory "src/Adapter/PostgreSQL/Migrations"]

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action =
  bracket initPool cleanPool action
  where
    initPool = createPool openConn closeConn
                (configStripeCount          cfg)
                (configIdleConnTimeout      cfg)
                (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn  = connectPostgreSQL (configUrl cfg)
    closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action  state

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

genvCode :: Text -> IO Text
genvCode email = do
  random <- stringRandomIO "[A-Za-z0-9]{16}"
  return $ (tshow email) <> "_" <> random

addAuth :: PG r m => D.Auth -> m (Either D.RegistrationError (D.UserId,D.VerificationCode))
addAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassw = D.rawPassword pass
  vCode  <- liftIO $ genvCode rawEmail
  result <- withConn $ \conn ->
    try $ query conn qry (rawEmail,rawPassw,vCode)
  case result of
    Right [Only uId] -> return $ Right (uId,vCode)
    Right _          -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg then
        return $ Left D.RegistrationErrorEmailTaken
      else
        throwString $ "Unhandled PG exception:" <> show err
  where
    qry = "insert into auths \
          \(email,pass,email_verification_code,is_email_verified) \
          \values (?, crypt(?,gen_salt('bf')),?,?) returning id"


setEmailAsVerified :: PG r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn ->
    query conn qry (Only vCode)
  case result of
    [(uId,email)] -> return $ makeEmail uId email
    _             -> return $ Left  D.EmailVerificationErrorInvalidCode
  where
    qry = "update auths set is_email_verified = true where email_verification_code = ?"
    makeEmail uId email =
      case D.mkEmail email of
        Right e -> Right (uId,e)
        _       -> Left  D.EmailVerificationErrorInvalidCode

-- my attempt without book...does not compile
-- setEmailAsVerified  :: PG r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId,D.Email))
-- setEmailAsVerified vCode = do
  -- authRes <- liftIO $ findAuthByVcode vCode
  -- case authRes of
    -- Left e     -> return $ Left D.EmailVerificationErrorInvalidCode
    -- Right [Only (uId,email)] -> do
      -- res2 <- liftIO $ updateAuthAsVerified email
      -- case res2 of
        -- Left  e -> return $ Left D.EmailVerificationErrorInvalidCode
        -- Right _ -> return $ Right (uId,email)
  -- where
    -- findAuthByVcode vCode =
      -- withConn $ \conn ->
        -- try $ query conn qry (vCode)
    -- qry = "select id,email from auths where email_verification_code = ? "
    -- updateAuthAsVerified email =
      -- withConn $ \conn ->
        -- try $ query conn qry2 (email)
    -- qry2 = "update \
           -- \  auths \
           -- \set\
           -- \  is_email_verified = true \
           -- \where\
           -- \  email = ?"

-- my attempt
findUserByAuth :: (PG r m) => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  res <- withConn $ \conn ->
    query conn qry (email,passw)
  return $
    case res of
      [(uId,isVerified)] -> Just (uId,isVerified)
      _                  -> Nothing
  where
    qry   = "select id, is_email_verified from auths where email = ? and pass = crypt(?,pass)"
    email = D.rawEmail    $ D.authEmail    auth
    passw = D.rawPassword $ D.authPassword auth


-- my attempt
findEmailFromUserId :: (PG r m) => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  res <- withConn $ \conn ->
    query conn qry (Only uId)
  case res of
    [Only email] -> return $ createEmail email
    _            -> return $ Nothing
  where
    qry = "select cast(email as text) from auths where id = ?"
    createEmail email =
      case (D.mkEmail email) of
        Right e -> Just e
        _       -> Nothing
