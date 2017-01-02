{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Microphone.Example where
import Microphone

import qualified Pipes.Prelude as P
--import Pipes

import Data.Int           (Int16)
--import Control.Concurrent (forkIO)
--import Control.Exception  (finally)
import System.Environment (getArgs)
import Data.Function ((&))
import Data.Maybe (listToMaybe)

{-|

@
stack build && stack exec -- example-microphone audio.l16

aplay --file-type wav --format=S16_LE --channels=1 --rate=16000 audio.l16
# "S16_LE" means "Signed / 16 bits per sample (i.e. Int16), little-endian"
# 'defaultMicrophoneConfig' uses a mono channel and a 16kHz sample rate
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
  putStrLn $ "(bytes: "++(show $ length audio)++")"

  where
  _getFilePath :: IO FilePath
  _getFilePath = do
    args <- getArgs
    return $ args & listToMaybe & maybe "audio.l16" id

-- |
listenUntilUserPressesReturn :: MicrophoneConfig Int16 -> IO [Int16]
listenUntilUserPressesReturn config = do

  environment <- listenMicrophone config  -- start

  -- TODO make separate example
  -- _ <- forkIO $ runEffect $ microphone environment >-> P.map show >-> P.stdoutLn
  -- Streaming works

  putStrLn "(listening...)"

  _ <- getLine                            -- blocks
  putStrLn "(silencing...)"
  silenceMicrophone environment           -- stop

  -- audio <- getMicrophoneContents environment
  audio <- P.toListM (microphone environment) -- TODO stops when silenced

  return audio
