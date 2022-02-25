module Network.TCP
  ( Socket(..)
  , TCPSerializable(..)
  , send
  , receive
  , connect
  , close
  , disconnect
  )
where

import qualified Network.TCP.Internals.Client  as S
import qualified Network.TCP.Internals.Server  as S
import           Network.TCP.Socket

connect :: TCPSerializable s => String -> Int -> IO (Socket s)
connect host port = do
  sock <- S.connect host port
  return $ Socket sock

send :: TCPSerializable s => Socket s -> s -> IO ()
send = socketSend

receive :: TCPSerializable s => Socket s -> IO (Maybe s)
receive = socketReceive

close = S.close . unSocket
disconnect = close
