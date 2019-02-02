{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Monoid ((<>))

import Pipes.Network.Client.Telnet

type Time  = String
type Date  = String
type Place = String

type MyState a = StateT WeatherState IO a

data WeatherState
  = Started
  | Requesting
  | ReceivedData Command
  deriving (Show, Eq)

data Command
  = Enter String
  | Input String
  | Menu String
  | Various String
  | Weather Time Date Place
  deriving (Eq, Show)

addr :: String
addr = "rainmaker.wunderground.com"

port :: String
port = "23"

-- this service has the following order instead of \r\n for some reason
weirdLine :: Parser ()
weirdLine = do
  char '\n'
  char '\r'
  return ()

cmdEnter :: Parser Command
cmdEnter = do
  t <- string "Press Return to continue:"
  return . Enter . unpack $ t

cmdMenu :: Parser Command
cmdMenu = do
  count 4 space
  t <- string "Press Return to continue, M to return to menu, X to exit:"
  return . Menu . unpack $ t

cmdInput :: Parser Command
cmdInput = do
  t0 <- string "Press Return for menu"
  char '\r'
  char '\n'
  t1 <- "or enter 3 letter forecast city code--"
  return . Input . unpack $ (t0 <> " " <> t1)

dataWeather :: Parser Command
dataWeather = do
  string " Weather Conditions at "
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  space
  x <- string "AM" <|> string "PM"
  space
  z <- takeTill (== ' ')
  string " on "
  d <- count 2 digit
  space
  n <- takeTill (== ' ')
  space
  y <- count 4 digit
  string " for "
  p <- takeTill (== '.')
  anyChar
  weirdLine
  return $ Weather (h <> ":" <> m <> " " <> unpack x <> " " <> unpack z)
                   (d <> " " <> unpack n <> " " <> y) (unpack p)

dataVarious :: Parser Command
dataVarious = do
  t <- manyTill anyChar weirdLine
  return . Various $ t

newLine :: Parser Command
newLine = do
  char '\r'
  char '\n'
  return $ Various ""

commandReader :: Parser Command
commandReader =
      newLine
  <|> cmdEnter
  <|> cmdMenu
  <|> cmdInput
  <|> dataWeather
  <|> dataVarious

commandHandler :: WeatherState -> Command -> MyState (WeatherState, Maybe ByteString)
commandHandler s Enter{}     = return (s, Just "\n")
commandHandler s Input{}     = updateState Requesting >> return (s, Just "anc\n")
commandHandler s Menu{}      = return (s, Just "X\n")
commandHandler s w@Weather{} = updateState (ReceivedData w) >> return (s, Nothing)
commandHandler s _           = return (s, Nothing)

updateState :: WeatherState -> MyState ()
updateState s = do
  put s
  io $ print s

main :: IO ()
main = void (runStateT code Started)

code :: MyState ()
code = do
  s <- get
  io $ print $ "Initial State: " ++ show s
  void (runTelnet addr port s commandReader commandHandler)
  s <- get
  io $ print $ "Final State: " ++ show s
  return ()

io :: IO a -> MyState a
io = liftIO
