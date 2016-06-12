{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, LambdaCase #-}
module Microphone.PortAudio where
import Microphone.Extra
import Microphone.Types

-- import Pipes.Concurrent
-- import Pipes

import Sound.PortAudio
import Sound.PortAudio.Base

--import Control.Concurrent (threadDelay)
-- import Control.Concurrent.STM
--import Control.Exception (throwIO)
-- import Foreign.C (CInt)

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

{-|

-}

--------------------------------------------------------------------------------

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

    Left e -> fail (show e) -- throwM e
    Right a -> return a

  return device

{-|

-}
withStream'
  :: (StreamFormat input, StreamFormat output)
 => OpenStream input output
 -> (Stream input output -> PortAudio a)
 -> PortAudio a

withStream' OpenStream{..}
  = withStream sInput sOutput sSampleRate sFramesPerBuffer sFlags sCallback sFinalizer

--------------------------------------------------------------------------------

