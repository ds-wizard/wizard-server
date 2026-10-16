module SharedTest.Specs.Api.Websocket where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import qualified Control.Exception as E
import Control.Monad (unless, void)
import qualified Data.Text as T
import Data.Word (Word16)
import Network.Socket
import Network.WebSockets
import Network.WebSockets.Stream hiding (close)
import System.Timeout (timeout)
import Test.Hspec

import Shared.Model.Websocket.WebsocketRegistry

receiveTimeoutMicros :: Int
receiveTimeoutMicros = 5000000

createConnectionOnPort :: Int -> String -> IO (Connection, Socket)
createConnectionOnPort port reqUrl = do
  sock <- openLoopbackSocket port
  stream <- makeSocketStream sock
  connection <- newClientConnection stream ("localhost:" ++ show port) reqUrl defaultConnectionOptions [] `E.onException` close sock
  return (connection, sock)

openLoopbackSocket :: Int -> IO Socket
openLoopbackSocket port = do
  sock <- socket AF_INET Stream 6
  setSocketOption sock NoDelay 1
  connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))) `E.onException` close sock
  return sock

closeConnectionAndSocket :: Connection -> Socket -> IO ()
closeConnectionAndSocket connection sock = do
  void . E.try @E.SomeException $ sendClose connection (T.pack "close")
  threadDelay 100000
  close sock

waitForWebsocketNode :: Int -> WebsocketRegistry -> Int -> IO ()
waitForWebsocketNode port registry listens = go (100 :: Int)
  where
    go 0 = expectationFailure "The websocket node did not become ready"
    go attempts = do
      listening <- (>= listens) <$> readTVarIO registry.listenCount
      accepting <- either (const False) (const True) <$> E.try @E.IOException (openLoopbackSocket port >>= close)
      unless (listening && accepting) (threadDelay 100000 >> go (attempts - 1))

receiveDataWithin :: WebSocketsData a => Connection -> IO a
receiveDataWithin connection = do
  mData <- timeout receiveTimeoutMicros (receiveData connection)
  maybe (expectationFailure "No websocket message received within 5 s" >> fail "timeout") return mData

receiveDataMessageWithin :: Connection -> IO DataMessage
receiveDataMessageWithin connection = do
  mMessage <- timeout receiveTimeoutMicros (receiveDataMessage connection)
  maybe (expectationFailure "No websocket message received within 5 s" >> fail "timeout") return mMessage

read_Closed :: Connection -> Word16 -> IO ()
read_Closed connection expCode = do
  result <- E.try (receiveDataMessageWithin connection)
  case result of
    Left (CloseRequest code _) -> code `shouldBe` expCode
    Left ConnectionClosed -> return ()
    other -> expectationFailure ("Expected a closed connection, got: " ++ show other)

read_CloseCode :: Connection -> Word16 -> IO ()
read_CloseCode connection expCode = do
  result <- E.try (receiveDataMessageWithin connection)
  case result of
    Left (CloseRequest code _) -> code `shouldBe` expCode
    other -> expectationFailure ("Expected a close frame, got: " ++ show other)

nothingWasReceived :: Connection -> IO ()
nothingWasReceived = nothingWasReceivedWithin 200000

nothingWasReceivedWithin :: Int -> Connection -> IO ()
nothingWasReceivedWithin micros connection = do
  mMessage <- timeout micros (receiveDataMessage connection)
  void mMessage `shouldBe` Nothing

read_Rejected :: Int -> String -> Int -> IO ()
read_Rejected port reqUrl expCode = do
  result <- E.try (createConnectionOnPort port reqUrl)
  case result of
    Left (MalformedResponse response _) -> responseCode response `shouldBe` expCode
    Left other -> expectationFailure ("Expected a rejected handshake, got: " ++ show other)
    Right (_, sock) -> close sock >> expectationFailure "Expected a rejected handshake, got an accepted one"

assertEventually :: (Eq a, Show a) => IO a -> a -> IO ()
assertEventually action expected = go (50 :: Int)
  where
    go 0 = action >>= (`shouldBe` expected)
    go attempts = do
      actual <- action
      unless (actual == expected) (threadDelay 100000 >> go (attempts - 1))
