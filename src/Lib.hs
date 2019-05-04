module Lib(someFunc) where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as Redis
import Domain.Auth
import Katip
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth    as MQAuth

import Text.StringRandom



type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App {

  unApp :: ReaderT State (KatipContextT IO) a

} deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip, MonadThrow, MonadCatch)


instance AuthRepo App where
  addAuth             = M.addAuth
  setEmailAsVerified  = M.setEmailAsVerified
  findUserByAuth      = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  newSession            = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId 

run :: LogEnv -> State -> App a -> IO a
run logEnv state =
  runKatipContextT logEnv () mempty 
  . flip runReaderT state
  . unApp

  
withState :: (LogEnv -> State -> IO ()) -> IO ()
withState action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action le state
  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    redisCfg = "redis://localhost:6379/0"
    pgCfg = PG.Config
      { PG.configUrl = "postgresql://webappuser:!Q2w3e4r5t@localhost/hauth"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.configIdleConnTimeout = 10
      }

main :: IO ()
main =
  withState $ \le state@(_,_,mqState,_) -> do
    let runner = run le state
    MQAuth.init mqState runner
    runner action

someFunc :: IO ()
someFunc = withKatip $ \logEnv -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> 
    Redis.withState redisCfg $ \redisState ->
      MQ.withState mqCfg 16 $ \mqState -> do
        let runner = run logEnv (pgState,redisState,mqState,mState) 
        MQAuth.init mqState runner
        runner action
  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    redisCfg = "redis://localhost:6379/0"
    pgCfg = 
      PG.Config 
        { PG.configUrl = "postgresql://webappuser:!Q2w3e4r5t@localhost/hauth"
        , PG.configStripeCount = 2
        , PG.configMaxOpenConnPerStripe = 5
        , PG.configIdleConnTimeout = 10
        }




action :: App ()
action = do
  randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@test\\.com"
  let email = either undefined id $ mkEmail randEmail
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth  = Auth email passw
  register auth
  vCode <- pollNotif email
  verifyEmail vCode
  Right session        <- login auth
  Just uId             <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- M.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode


runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv       <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Login in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Login in ns2"
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" $ asText "12") $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context"
        katipAddContext (sl "contry" $ asText "Sinapore") $
          $(logTM) InfoS "Log in ns2.ns3 with userId and country context"
