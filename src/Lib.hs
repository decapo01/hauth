module Lib
    ( someFunc
    ) where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth

type State = TVar M.State
newtype App a = App {
  unApp :: ReaderT State IO a

} deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)


instance AuthRepo App where
  addAuth             = M.addAuth
  setEmailAsVerified  = M.setEmailAsVerified
  findUserByAuth      = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId 

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

someFunc :: IO ()
someFunc = putStrLn "someFunc"
