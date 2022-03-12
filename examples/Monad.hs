{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Socket
import           Control.Monad.IO.Class
import           System.Environment

data Message
  = Hello
  | Bye

instance Packet Message where
  serialize Hello = "hello"
  serialize Bye   = "bye"

  deserialize "hello" = Just Hello
  deserialize "bye"   = Just Bye
  deserialize _       = Nothing

server = do
  sock <- listen 9290 :: IO (Socket Message)
  forever $ do
    acceptSocketT sock $ do
      liftIO $ putStrLn "new client"
      Just Hello <- receive
      liftIO $ putStrLn "saying goodbye"
      send Bye
    putStrLn "client closed"

client :: IO ()
client = connectSocketT "localhost" 9290 $ do
  liftIO $ putStrLn "connected. saying hi"
  send Hello
  Just Bye <- receive
  liftIO $ putStrLn "response received. closing..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["client"] -> client
    ["server"] -> server
    _          -> putStrLn "usage: tcp-low-level-demo client | server"

