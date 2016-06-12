{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Microphone.Example where
import Microphone

import Data.Int           (Int16)
-- import Control.Concurrent (forkIO)
import Control.Exception  (finally)

{-|
@
stack build && stack exec -- example-microphone
@
-}
main = do

  -- withPortAudio $ do
  --    n <- getNumDevices -- >>= either (fail.show) return
  --    print n
  --    return $ Right ()
  -- return ()

  audio <- listenUntilUserPressesReturn defaultMicrophoneConfig

  print audio `finally` do -- So you can still see the length after a user interrupt, without scrolling up
      print $ length audio

-- |
listenUntilUserPressesReturn :: MicrophoneConfig Int16 -> IO [Int16]
listenUntilUserPressesReturn config = do

  microphone <- listenMicrophone config  -- start
  putStrLn "(listening...)"

  _ <- getLine                           -- blocks
  putStrLn "(silencing...)"
  silenceMicrophone microphone           -- stop

  audio <- getMicrophoneContents microphone
  return audio
