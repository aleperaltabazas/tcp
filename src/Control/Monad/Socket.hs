{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Control.Monad.TCP
  ( SocketT
  , receive
  , send
  , connect
  , disconnect
  , runCleanSocketT
  , runSocketT
  )
where

import           Control.Monad.Reader
import qualified Network.TCP                   as TCP

newtype SocketT s m a
  = SocketT (ReaderT (TCP.Socket s) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

receive :: (Monad m, MonadIO m, TCP.TCPSerializable s) => SocketT s m (Maybe s)
receive = SocketT $ do
  sock <- ask
  liftIO $ TCP.receive sock

send :: (Monad m, MonadIO m, TCP.TCPSerializable s) => s -> SocketT s m ()
send m = SocketT $ do
  sock <- ask
  liftIO $ TCP.send sock m

runCleanSocketT
  :: (TCP.TCPSerializable s, MonadIO m) => String -> Int -> SocketT s m a -> m a
runCleanSocketT host port sockT = do
  sock <- liftIO $ connect host port
  a    <- runSocketT sock sockT
  liftIO $ disconnect sock
  return a

connect :: TCP.TCPSerializable s => String -> Int -> IO (TCP.Socket s)
connect = TCP.connect

disconnect :: TCP.Socket s -> IO ()
disconnect = TCP.disconnect

runSocketT :: (TCP.TCPSerializable s) => TCP.Socket s -> SocketT s m a -> m a
runSocketT sock (SocketT r) = runReaderT r sock
