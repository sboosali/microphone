{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, LambdaCase #-}
{-|

e.g. Non-streaming. Get the raw audio data from a microphone session:

@
import "Microphone.Streaming"
import "Control.Concurrent" ('forkIO')
import "Data.Int"           ('Data.Int.Int16')

main = do
  audio <- 'Microphone.Example.listenUntilUserPressesReturn' 'defaultMicrophoneConfig'
  print audio

listenUntilUserPressesReturn :: 'MicrophoneConfig' Int16 -> IO [Int16]
listenUntilUserPressesReturn config = do

 microphone <- 'listenMicrophone' config  -- start

 _ <- forkIO $ do
     _ <- 'getLine'                       -- blocks
     'silenceMicrophone' microphone       -- stop

 audio <- 'getMicrophoneContents' microphone
 return audio
@

e.g. Streaming. Get chunks of raw audio data from a microphone:

@
TODO
@

See "Microphone.Example", these examples are built with this package.

(the @portaudio@ library is symmetric with respect to input/output device. So, the functions here should work for writing audio to the "speakers", with some minimal modifications. e.g. reading from instead of writing to the channel.)

-}
module Microphone.Streaming where
import Microphone.Extra
import Microphone.Types
import Microphone.PortAudio

-- import Pipes.Concurrent
import Pipes

import Sound.PortAudio
import Sound.PortAudio.Base

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Foreign (ForeignPtr, peekArray,withForeignPtr, mallocForeignPtrArray)
--import qualified Data.Sequence as Sequence
import Data.Sequence (Seq)
import System.IO

{-|

-}

--------------------------------------------------------------------------------

{-| A microphone produces audio (in chunks).

-}
microphone :: MicrophoneEnvironment i -> Producer i IO ()
microphone environment = loop
 where
 loop = do
   liftSTM (peekMicrophone environment) >>= \case
     Nothing -> return() -- done
     Just is -> do
         each is
         loop            -- continue

{-TODO

Producer i IO ()

Producer [i] IO () with pipes-group


-}


{-|

-}
peekMicrophone :: MicrophoneEnvironment i -> STM (Maybe [i])
peekMicrophone MicrophoneEnvironment{..} = do
   readTChan mChannel --NOTE blocks

 --TODO race condition where The producer blocksOn a clothes microphone?
--PROVE mChannel never blocks after mSwitch? Rather, it returns a Nothing.

{-old
  is <- liftSTM $ do
      readTVar mSwitch >>= \case
        False -> return []
        True  -> readTChan mChannel
  each is
-}

-- {-| 

-- blocks until 'silenceMicrophone' (i.e. /not/ lazy-IO i.e. unlike 'getContents').

-- TODO Use pipes For streaming

-- -}
-- getMicrophoneContents :: forall i. MicrophoneEnvironment i -> IO [i] -- TODO Array. Or expose seq? What can be efficiently converted to bytestring? Several seconds of audio is about hundreds of thousands of bytes.
-- getMicrophoneContents MicrophoneEnvironment{..} = toList <$> go Sequence.empty
--   where
--   go :: Seq i -> IO (Seq i)
--   go is = do
--     (atomically $ tryReadTChan mChannel) >>= \case
--       Nothing -> do
--         return is
--       Just js -> do
--         go (is <> Sequence.fromList js) -- right-append

  --old
  -- () <- whileMicrophoneOn environment (pause 1) -- block, checking every millisecond
  -- whileJustM $ tryReadTChan mChannel
  -- return []

{-| Start listening to the microphone.

Forks a thread: a "bracket" that acquires portaudio and an input stream, and then releases them on 'silenceMicrophone'; calls 'writingMicrophone' with the acquired stream.

-} -- openMicrophone
listenMicrophone :: (StreamFormat i) => MicrophoneConfig i -> IO (MicrophoneEnvironment i)
listenMicrophone config = do

  environment <- newMicrophoneEnvironment config

  _ <- forkIO $ void $ withPortAudio $ do
      streamConfig <- fromMicrophoneConfig config
      withStream' streamConfig $ \stream -> do -- TODO managed
          Nothing <- startStream stream -- TODO Partial
          _ <- writingMicrophone environment stream
          Nothing <- stopStream stream -- TODO Needed?
          return OK

  return environment

{-| Stop listening to the microphone.

terminate @portaudio@ by setting 'mSwitch' TODO

Mark that the stream is done with:

@
'mChannel' `'writeTChan'` 'Nothing'
@

-} -- closeMicrophone
silenceMicrophone :: MicrophoneEnvironment i -> IO ()
silenceMicrophone MicrophoneEnvironment{..} = do
  atomically $ do
      mSwitch `writeTVar` False -- TODO necessary?
      mChannel `writeTChan` Nothing -- TODO close a TChan?

--------------------------------------------------------------------------------

{-| read some data from the 'mChannel'. even if it's closed (returning @[]@).

blocking.

-}
readMicrophone :: MicrophoneEnvironment i -> IO [i]
readMicrophone MicrophoneEnvironment{..} = atomically $ do
  maybe [] id <$> readTChan mChannel

{-| loops, calling 'writeMicrophone'. aborts after 'silenceMicrophone' is called.

-}
writingMicrophone :: (StreamFormat i) => MicrophoneEnvironment i -> Stream i i -> PortAudio ()
writingMicrophone environment stream = do
  _ <- whileMicrophoneOn environment $ do
    pause 10
    _displayVolume stream
    void $ writeMicrophone environment stream
  return OK

_displayVolume :: Stream i i -> IO () --TODO Actually display the volume, and purify. Maybe with bidirectional pipes? 
_displayVolume _ = _display "."
  where
  _display x = putStr x >> putStr " " >> hFlush stdout

-- |
whileMicrophoneOn :: MicrophoneEnvironment i -> (IO () -> IO ())
whileMicrophoneOn environment m =
  whileM (environment&isMicrophoneOn) m

{-|

-}
isMicrophoneOn :: MicrophoneEnvironment i -> IO Bool
isMicrophoneOn MicrophoneEnvironment{..} = do
  atomically $ readTVar mSwitch

{-| read from the portaudio buffer, and send it to the channel

-}
writeMicrophone :: (StreamFormat i) => MicrophoneEnvironment i -> Stream i i -> PortAudio ()
writeMicrophone MicrophoneEnvironment{..} stream = do
    Right nSamples <- readAvailable stream --TODO partial
    let nChannels = fromIntegral $ mConfig&mChannelCount
    let size = nChannels * nSamples

    --DEBUG nSamples & \x -> if x >= 0 then putStr (show x) >> putStr " " >> hFlush stdout else nothing

    when (nSamples >= 0) $ do -- skip consumption, when there's no data
        buffer <- mallocForeignPtrArray size
            --NOTE mallocForeignPtrArray is resource-safe
        _ <- readStream stream (fromIntegral nSamples) buffer
        sendMicrophone mChannel (size,buffer)
        return ()

    return OK

--old     unless (size <= 0)

{-|

-}
sendMicrophone :: (StreamFormat i) => MicrophoneChannel i -> (Int, ForeignPtr i) -> IO ()
sendMicrophone channel (size,_buffer) = withForeignPtr _buffer $ \buffer -> do
  is <- peekArray size buffer
--  print $ length is
  atomically $ writeTChan channel (Just is)
  -- StreamFormat subclasses Storable

--old   traverse_ print is -- needs Show i

-- TODO Stream i Void i.e. ignore speakers. Like type-level Nothing.

--------------------------------------------------------------------------------

-- | (new mutable variables)
newMicrophoneEnvironment :: MicrophoneConfig i -> IO (MicrophoneEnvironment i)
newMicrophoneEnvironment mConfig = do
  mChannel <- newTChanIO
  mSwitch <- newTVarIO True
  return MicrophoneEnvironment{..}

fromMicrophoneConfig
  :: (StreamFormat i)
  => MicrophoneConfig i
  -> IO (OpenStream i i)
fromMicrophoneConfig MicrophoneConfig{..} = do

  device <- mInputDevice & maybe
      getDefaultInputStream
      return

  let spChannelCount = mChannelCount&fromIntegral
  let sInput = Just (defaultStreamParameters device){spChannelCount}

  return $ defaultOpenStream
   { sSampleRate = mSampleRate
   , sFlags = mFlags
   , sInput
   }

getDefaultInputStream :: IO PaDeviceIndex --TODO PortAudio
getDefaultInputStream = do
  (device,_) <- getDefaultInputInfo >>= \case --  (defaultInputDevice,_) <- getDefaultInputInfo --TODO EitherT

    Left e -> fail (show e) -- TODO throwM e
    Right a -> return a

  return device

--------------------------------------------------------------------------------

{-|

@
stack build && stack exec -- it portaudio
@

@
brew install portaudio

ls /usr/local/opt/portaudio/lib
@

more examples:

https://github.com/sw17ch/portaudio/tree/master/examples

The microphone data is formatted as PCM?

haskell FLAC?


flac:

http://superuser.com/questions/553503/what-is-the-difference-between-wav-and-flac

"""
FLAC is a lossless audio codec (its container also happens to be called FLAC, but the main idea here is the actual codec).
WAV, on the other hand, as a container can hold numerous kinds of audio codecs, but mostly, you'll find PCM-encoded audio.
So, simply put: Take a WAV file with PCM-encoded audio, and the corresponding (mathematically equal) FLAC file will be a tad smaller. The downside is that FLAC is not as widely supported as WAV. For example, most (all?) operating systems won't play or convert FLAC files without extra software.
"""

@
flac  --channels 1  --sample-rate 16000  --endian little  --sign signed  --bps 16  -o microphone.flac  --force-raw-format  microphone.wav
@

because:

* portaudio sets the number of channels and the sample rate.
* printing the buffer, I see negatives. samples are Int16.
* the architecture of my machine is Intel (About This Mac: 1.8 GHz Intel Core i5)
* bits per sample: samples are Int16.

@
flac  --channels 1  --sample-rate 16000  --endian little  --sign signed  --bps 8 -o final.flac  --force-raw-format  final.wav
@

@
$ du *
# the size of each file in the current directory, in bytes
@

RAW Audio format or just RAW Audio is an audio file format for storing uncompressed audio in raw form. Comparable to WAV or AIFF in size, RAW Audio file does not include any header information (sampling rate, bit depth, endian, or number of channels). Data can be written in PCM, IEEE 754 or ASCII.[citation needed]


-}

{-
readStream  :: Stream input output -> CULong -> ForeignPtr input  -> IO ()
writeStream :: Stream input output -> CULong -> ForeignPtr output -> IO ()

ForeignPtr a
its length must be the number of frames times the channels in the underlying stream

mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)

"It uses pinned memory in the garbage collected heap, so the ForeignPtr does not require a finalizer to free the memory. Use of mallocForeignPtr and associated functions is strongly recommended in preference to newForeignPtr with a finalizer."
-}

{-(TODO like 'getContents'?)

no, getContents calls hGetContents calls lazyRead calls unsafeInterleaveIO

http://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.IO.Handle.Text.html#lazyRead

-}

--------------------------------------------------------------------------------
