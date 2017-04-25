{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Monoid ((<>))

import Pipes.Network.Client.Telnet

type Time  = String
type Date  = String
type Place = String

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

commandHandler :: () -> Command -> ((), Maybe ByteString)
commandHandler s Enter{}   = (s, Just "\n")
commandHandler s Input{}   = (s, Just "anc\n")
commandHandler s Menu{}    = (s, Just "x\n")
commandHandler s Weather{} = (s, Nothing)
commandHandler s _         = (s, Nothing)

main :: IO ()
main = runTelnet addr port () commandReader commandHandler >>= print
