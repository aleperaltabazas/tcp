module Network.TCP.Internals.Server
  ( bindTo
  , receive
  )
where

import qualified Data.ByteString.Lazy          as BL
import           Data.Int                       ( Int
                                                , Int64
                                                )
import           Network.Socket                 ( AddrInfo
                                                  ( addrFamily
                                                  , addrSocketType
                                                  , addrProtocol
                                                  , addrAddress
                                                  )
                                                , setCloseOnExecIfNeeded
                                                , withSocketsDo
                                                , setSocketOption
                                                , bind
                                                , socket
                                                , withFdSocket
                                                , SocketOption(ReuseAddr)
                                                , Socket
                                                )
import           Network.Socket.ByteString.Lazy ( recv )
import           Network.TCP.Internals.Address  ( resolveAddress )

bindTo :: Int -> IO Socket
bindTo port = withSocketsDo $ do
  addr <- resolveAddress Nothing port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  return sock

receive :: Int64 -> Socket -> IO BL.ByteString
receive = flip recv
