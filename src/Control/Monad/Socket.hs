{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Control.Monad.Socket
  ( SocketT
  , TCP.Socket
  , receive
  , send
  , connect
  , disconnect
  , listen
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

connect :: TCP.TCPSerializable s => String -> Int -> IO (TCP.Socket s)
connect = TCP.connect

disconnect :: TCP.Socket s -> IO ()
disconnect = TCP.disconnect

runSocketT
  :: (TCP.TCPSerializable s, MonadIO m) => TCP.Socket s -> SocketT s m a -> m a
runSocketT sock (SocketT r) = runReaderT r sock

listen :: TCP.TCPSerializable s => Int -> IO (TCP.Socket s)
listen = TCP.listen
