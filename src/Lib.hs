module Lib(someFunc) where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth

import Katip

type State = TVar M.State

newtype App a = App {

  unApp :: ReaderT State (KatipContextT IO) a

} deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip)



instance AuthRepo App where
  addAuth             = M.addAuth
  setEmailAsVerified  = M.setEmailAsVerified
  findUserByAuth      = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession            = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId 

run :: LogEnv -> State -> App a -> IO a
run logEnv state =
  runKatipContextT logEnv () mempty 
  . flip runReaderT state
  . unApp



someFunc :: IO ()
someFunc = withKatip $ \logEnv -> do
  state <- newTVarIO M.initialState
  run logEnv state action




action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth  = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session        <- login auth
  Just uId             <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)


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
