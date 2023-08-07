module Network.TCP
  ( Socket(..)
  , Packet(..)
  , send
  , receive
  , connect
  , close
  , disconnect
  , listen
  , accept
  , startTCPServer
  )
where

import           Control.Monad                (forever)
import qualified Network.TCP.Internals.Client as S
import qualified Network.TCP.Internals.Server as S
import           Network.TCP.Socket

connect :: Packet s => String -> Int -> IO (Socket s)
connect host port = do
  sock <- S.connect host port
  return $ Socket sock

send :: Packet s => Socket s -> s -> IO ()
send = socketSend

receive :: Packet s => Socket s -> IO (Maybe s)
receive = socketReceive

close = S.close . unSocket
disconnect = close

listen :: Packet s => Int -> IO (Socket s)
listen = socketListen

accept :: Packet s => Socket s -> IO (Socket s)
accept = socketAccept

startTCPServer :: Packet s => Int -> (Socket s -> IO ()) -> IO ()
startTCPServer port handleNewClient = do
  sock <- listen port
  forever $ do
    client <- accept sock
    handleNewClient client
