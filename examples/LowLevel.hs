{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Network.TCP
import           System.Environment

data Message
  = Hello
  | Bye

instance Packet Message where
  serialize Hello = "hello"
  serialize Bye   = "bye"

  deserialize "hello" = Just Hello
  deserialize "bye"   = Just Bye

server = do
  sock <- listen 9290 :: IO (Socket Message)
  forever $ do
    client <- accept sock
    putStrLn "new client"
    Just Hello <- receive client
    putStrLn "saying goodbye"
    send client Bye
    close client
    putStrLn "client closed"

client = do
  sock <- connect "localhost" 9290 :: IO (Socket Message)
  putStrLn "connected. saying hi"
  send sock Hello
  Just Bye <- receive sock
  putStrLn "response received. closing..."
  close sock

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["client"] -> client
    ["server"] -> server
