module Network.TCP.Internals.Address
  ( resolveAddress
  )
where

import           Network.Socket                 ( defaultHints
                                                , getAddrInfo
                                                , AddrInfo
                                                  ( addrFlags
                                                  , addrSocketType
                                                  )
                                                , AddrInfoFlag(AI_PASSIVE)
                                                , SocketType(Stream)
                                                )

resolveAddress :: Maybe String -> Int -> IO AddrInfo
resolveAddress host port = do
  let hints =
        defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) host (Just $ show port)
