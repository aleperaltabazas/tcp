{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Control.Monad.Socket
  ( SocketT
  , TCP.Socket
  , TCP.Packet(..)
  , receive
  , send
  , connect
  , disconnect
  , runSocketT
  , connectSocketT
  , acceptSocketT
  , TCP.listen
  , TCP.accept
  , TCP.close
  )
where

import           Control.Monad.Reader
import qualified Network.TCP                   as TCP

newtype SocketT s m a
  = SocketT (ReaderT (TCP.Socket s) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

receive :: (Monad m, MonadIO m, TCP.Packet s) => SocketT s m (Maybe s)
receive = SocketT $ do
  sock <- ask
  liftIO $ TCP.receive sock

send :: (Monad m, MonadIO m, TCP.Packet s) => s -> SocketT s m ()
send m = SocketT $ do
  sock <- ask
  liftIO $ TCP.send sock m

connect :: TCP.Packet s => String -> Int -> IO (TCP.Socket s)
connect = TCP.connect

disconnect :: TCP.Socket s -> IO ()
disconnect = TCP.disconnect

runSocketT :: (TCP.Packet s, MonadIO m) => TCP.Socket s -> SocketT s m a -> m a
runSocketT sock (SocketT r) = runReaderT r sock

connectSocketT
  :: (TCP.Packet s, MonadIO m) => String -> Int -> SocketT s m a -> m a
connectSocketT host port s = do
  sock <- liftIO $ connect host port
  res  <- runSocketT sock s
  liftIO $ disconnect sock
  return res

acceptSocketT
  :: (TCP.Packet s, MonadIO m) => TCP.Socket s -> SocketT s m a -> m a
acceptSocketT sock s = do
  client <- liftIO $ TCP.accept sock
  res    <- runSocketT client s
  liftIO $ disconnect client
  return res
