{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Microphone.Example where
import Microphone

import Data.Int           (Int16)
-- import Control.Concurrent (forkIO)
--import Control.Exception  (finally)
import System.Environment (getArgs)
import Data.Function ((&))
import Data.Maybe (listToMaybe)

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

  path <- _getFilePath

  audio <- listenUntilUserPressesReturn defaultMicrophoneConfig

  putStrLn "(saving...)"
  saveAudio_LINEAR16 path audio
  putStrLn $ "(saved: "++path++")"

  where
  _getFilePath :: IO FilePath
  _getFilePath = do
    args <- getArgs
    return $ args & listToMaybe & maybe "microphone.l16" id

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
  