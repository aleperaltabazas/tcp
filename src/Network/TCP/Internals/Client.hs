module Network.TCP.Internals.Client
  ( connect
  , send
  , close
  )
where

import qualified Data.ByteString.Lazy          as BL
import           Network.Socket                 ( withSocketsDo
                                                , socket
                                                , AddrInfo
                                                  ( addrFamily
                                                  , addrSocketType
                                                  , addrProtocol
                                                  , addrAddress
                                                  )
                                                , Socket
                                                )
import qualified Network.Socket                as Socket
import qualified Network.Socket.ByteString.Lazy
                                               as Socket
import qualified Data.ByteString               as BS
import           Network.TCP.Internals.Address  ( resolveAddress )

connect :: String -> Int -> IO Socket
connect host port = withSocketsDo $ do
  addr <- resolveAddress (Just host) port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Socket.connect sock $ addrAddress addr
  return sock

send :: Socket -> BL.ByteString -> IO ()
send = Socket.sendAll

close = flip Socket.gracefulClose 10000
