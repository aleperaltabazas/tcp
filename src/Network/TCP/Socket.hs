module Network.TCP.Socket
  ( Socket(..)
  , TCPSerializable(..)
  , socketReceive
  , socketSend
  )
where

import qualified Network.Socket                as S
import qualified Network.TCP.Internals.Client  as S
import qualified Network.TCP.Internals.Server  as S
import qualified Data.Binary                   as Binary
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import           Data.Int                       ( Int64 )

newtype Socket s
  = Socket
  { unSocket :: S.Socket
  }

class TCPSerializable s where
  serialize  :: s -> BL.ByteString
  deserialize :: BL.ByteString  -> Maybe s

instance TCPSerializable BL.ByteString where
  serialize   = id
  deserialize = Just

socketSend :: TCPSerializable s => Socket s -> s -> IO ()
socketSend (Socket socket) s = do
  let bytes = serialize s
  let len   = BL.length bytes
  let size  = Binary.encode len
  let buf   = size <> bytes
  S.send socket buf

socketReceive :: TCPSerializable s => Socket s -> IO (Maybe s)
socketReceive (Socket socket) = do
  sizeBuf <- S.receive int64Size socket
  let size = Binary.decode sizeBuf :: Int64
  buf <- S.receive size socket
  return $ deserialize buf
  where int64Size = 8
