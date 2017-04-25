{-| A programmable telnet client built with @pipes@ and @attoparsec@ -}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipes.Network.Client.Telnet
  ( runTelnet
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forever)
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (pack, singleton)
import Data.Foldable (forM_)
import Data.Word (Word8)
import qualified Network.Simple.TCP as N
import Pipes
import Pipes.Prelude (stdoutLn)
import Pipes.Attoparsec (parse)
import Pipes.ByteString (ByteString)
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.Parse (runStateT)
import Pipes.Safe (MonadMask, runSafeT)

newtype TelnetError = TelnetError String deriving (Show)

data Cmd
  = SE | NOP | DM | BRK | IP | AO | AYT | EC | EL | GA | SB
  | DO | DONT | WILL | WONT | IAC | Misc Word8 deriving (Eq, Show)

cmdToCode :: Cmd -> Word8
cmdToCode SE       = 240
cmdToCode NOP      = 241
cmdToCode DM       = 242
cmdToCode BRK      = 243
cmdToCode IP       = 244
cmdToCode AO       = 245
cmdToCode AYT      = 246
cmdToCode EC       = 247
cmdToCode EL       = 248
cmdToCode GA       = 249
cmdToCode SB       = 250
cmdToCode DO       = 251
cmdToCode DONT     = 252
cmdToCode WILL     = 253
cmdToCode WONT     = 254
cmdToCode IAC      = 255
cmdToCode (Misc w) = w  

codeToCmd :: Word8 -> Cmd
codeToCmd 240 = SE
codeToCmd 241 = NOP
codeToCmd 242 = DM
codeToCmd 243 = BRK
codeToCmd 244 = IP
codeToCmd 245 = AO
codeToCmd 246 = AYT
codeToCmd 247 = EC
codeToCmd 248 = EL
codeToCmd 249 = GA
codeToCmd 250 = SB
codeToCmd 251 = DO
codeToCmd 252 = DONT
codeToCmd 253 = WILL
codeToCmd 254 = WONT
codeToCmd 255 = IAC
codeToCmd w   = Misc w

-- Telnet verb.
--
-- e.g. 255(IAC),251(WILL),3
verb :: A.Parser [Cmd]
verb = do
  A.word8 255
  v <- A.word8 251 <|> A.word8 252 <|> A.word8 253 <|> A.word8 254
  c <- A.anyWord8
  return [IAC, codeToCmd v, codeToCmd c]

-- Sub negotiation.
--
-- e.g. IAC,SB,<option code number>,1,IAC,SE
neg :: A.Parser [Cmd]
neg = do
  A.word8 255
  A.word8 250
  c <- A.anyWord8
  A.word8 1
  a <- A.anyWord8
  A.word8 255
  A.word8 240
  return [IAC, SB, codeToCmd c, Misc 1, codeToCmd a, IAC, SE]

-- Unmatched data gets forwarded along.
pass :: A.Parser [Cmd]
pass = do
  w <- A.anyWord8
  return [codeToCmd w]

parseTelnet :: A.Parser [Cmd]
parseTelnet = verb <|> neg <|> pass

squash :: [Cmd] -> Maybe ByteString
squash [] = Nothing
squash cs = Just . pack . fmap cmdToCode $ cs

-- Given a command list, return a tuple of (bytes to forward, bytes to send).
-- Forwarded bytes go to the next parser, sent bytes go out the network socket.
--
-- These states are not being tracked and simply notify a server that requires
-- this kind of exchange that the client is behaving (but not really).
--
-- TODO: This needs more extensive testing.
telnetHandler :: [Cmd] -> (Maybe ByteString, Maybe ByteString)
telnetHandler [Misc w]            = (Just . singleton $ w, Nothing)
telnetHandler [IAC, WILL, Misc a] = (Nothing, squash [IAC, WILL, Misc a])
telnetHandler [IAC, DO,   Misc a] = (Nothing, squash [IAC, DO  , Misc a])
telnetHandler [IAC, WONT, Misc a] = (Nothing, squash [IAC, WONT, Misc a])
telnetHandler [IAC, DONT, Misc a] = (Nothing, squash [IAC, DONT, Misc a])
telnetHandler [IAC, SB,   Misc a, Misc 1, IAC, SB] =
  (Nothing, squash [IAC, SB, Misc a, Misc 0, IAC, SE]) -- not exactly valid
telnetHandler _ = (Nothing, Nothing)

{-| 'runTelnet' takes an initial state, an attoparsec parser, and a handler
    which will both maintain application specific state and emit the correct
    commands to the server based on the input and/or current state.
    
    Returns either a 'TelnetError' on failure, or '()' on success.
-}
runTelnet :: (MonadIO m, MonadMask m, Show a)
          => String                            -- Host
          -> String                            -- Port
          -> s                                 -- Initial State
          -> A.Parser a                        -- Input Parser
          -> (s -> a -> (s, Maybe ByteString)) -- Output / State Handler
          -> m (Either TelnetError ())
runTelnet host port s i o =
  N.connect host port $ \(sock, _) -> do
    r <- runSafeT $ runEffect $
      run (codes sock $ fromSocket sock 4096) >-> sout >-> emit sock s
    N.closeSock sock
    return r
  where
    -- First scan the stream for valid telnet codes.
    -- Forward actual data to the outer parser, otherwise output to the socket.
    codes sock p = do
      (m, p') <- runStateT (parse parseTelnet) p
      case m of
        Nothing -> return $ Right ()
        Just e  ->
          case e of
            Left err -> return . Left . TelnetError . show $ err
            Right v0 -> do
              let (fwd, out) = telnetHandler v0
              forM_ out yield >-> toSocket sock
              forM_ fwd yield >> codes sock p'
         
    -- Run the outter application specific parser.
    -- Pass the parsed data forward.
    run p = do
      (m, p') <- runStateT (parse i) p
      case m of
        Nothing -> return $ Right ()
        Just e  ->
          case e of
            Left err -> return . Left . TelnetError . show $ err
            Right v  -> yield v >> run p'

    -- Output the data passed along to stdout.
    sout = forever $ do
      v <- await
      yield (show v) >-> stdoutLn
      yield v

    -- Update the application state with the parsed command and see if it emits
    -- a command to send along to the socket.
    emit sock s0 = do
      v0 <- await
      let (s1, v1) = o s0 v0
      forM_ v1 yield >-> toSocket sock
      emit sock s1
