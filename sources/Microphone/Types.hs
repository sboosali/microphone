{-# LANGUAGE RecordWildCards, OverloadedLists, ScopedTypeVariables, PatternSynonyms, DeriveAnyClass #-}

module Microphone.Types where
import Microphone.Orphans()
import Microphone.Extra

import Sound.PortAudio hiding (Error)
import qualified Sound.PortAudio as PortAudio
import Sound.PortAudio.Base

-- import Data.Set (Set)
-- import Data.Int (Int8) -- (Int16): with Int16, readStream/writeStream always succeed
-- import Data.Word (Word8)
import Control.Concurrent.STM
import Numeric.Natural

--------------------------------------------------------------------------------

{-|

-}
data MicrophoneEnvironment i = MicrophoneEnvironment
 { mConfig :: MicrophoneConfig i -- ^ 
 , mSwitch :: TVar Bool -- ^ @False@ turns the microphone off TODO rm
 , mChannel :: MicrophoneChannel i  -- ^ a buffered stream of audio data
 }
-- TODO TChan (Maybe [i]) ??? For The pipes producer.

type MicrophoneChannel i = TChan (Maybe [i])-- TMChan [i]


--TODO rename to microphone

{-| a "simple" subset of 'OpenStream'.

The phantom parameter @i@ is like a type-level field.
It should be a 'StreamFormat', one of:

* Int8
* Int16
* Int32
* CFloat

-}
data MicrophoneConfig i = MicrophoneConfig
 { mSampleRate :: Double
 , mChannelCount :: Natural
 , mFlags :: [StreamOpenFlag] -- ^ a Set
 , mInputDevice :: InputDevice -- ^ @Nothing@ means the default input device. e.g. the built-in microphone.
 }
 deriving (Show,Read,Eq,Ord,Generic,Hashable,NFData) -- TODO Data

-- |
type InputDevice = Maybe PaDeviceIndex

{-| all @portaudio@ functions run under this monad.

TODO EitherT PortAudio.Error IO

-}
type PortAudio a = IO (Either PortAudio.Error a)

{-|

e.g.

@
defaultOpenStream  = OpenStream input ouptut
defaultOpenStream  = OpenStream{..}
  where
  sInput           = Nothing
  sOutput          = Nothing
  sSampleRate      = 16000
  sFramesPerBuffer = Nothing
  sFlags           = []
  sCallback        = Nothing
  sFinalizer       = Nothing
@

Naming: like @OpenProcess@.

-}
data OpenStream input output = OpenStream
 { sInput :: Maybe (StreamParameters input)         -- ^ Stream Parameters for Input Device,
                                                    -- or Nothing if we don't need Audio input for this Stream

 , sOutput :: Maybe (StreamParameters output)       -- ^ Stream Parameters for Output Device,
                                                    -- or Nothing if we don't need Audio output for this Stream

 , sSampleRate :: Double                            -- ^ Sample Rate for Input and Output Devices,
                                                    -- if you need distinct Sample Rates for input and output, then you should create seperate Streams

 , sFramesPerBuffer :: Maybe Int                    -- ^ Requested Frames Per Buffer,
                                                    -- or Nothing if you'd prefer the underlying library to send you the amount it sees fit

 , sFlags :: [StreamOpenFlag]                       -- ^ Various Parameters Dictating the Operation of the Callback Function

 , sCallback :: Maybe (StreamCallback input output) -- ^ Callback,
                                                    -- or Nothing for a blocking read/write stream

 , sFinalizer :: Maybe FinCallback                  -- ^ Callback on Completion,
                                                    -- or Nothing if no final processing necessary
 }

--------------------------------------------------------------------------------

{-|

@
  'mSampleRate' = 'DEFAULT_SAMPLE_RATE'
  'mChannelCount' = 'DEFAULT_CHANNEL_COUNT'
  'mInputDevice' = 'DEFAULT_INPUT_DEVICE'
  'mFlags'      = []
@

-}
defaultMicrophoneConfig :: MicrophoneConfig i
defaultMicrophoneConfig = MicrophoneConfig{..}
  where
  mSampleRate   = DEFAULT_SAMPLE_RATE
  mChannelCount = DEFAULT_CHANNEL_COUNT
  mInputDevice  = DEFAULT_INPUT_DEVICE
  mFlags        = []

{-|
@
  'sInput'           = Nothing
  'sOutput'          = Nothing
  'sSampleRate'      = 44100
  'sFramesPerBuffer' = Nothing
  'sFlags'           = []
  'sCallback'        = Nothing
  'sFinalizer'       = Nothing
@
-}
defaultOpenStream :: OpenStream input ouptut
defaultOpenStream  = OpenStream{..}
  where
  sInput           = Nothing
  sOutput          = Nothing
  sSampleRate      = DEFAULT_SAMPLE_RATE
  sFramesPerBuffer = Nothing -- e.g. Just 0x800
  sFlags           = []
  sCallback        = Nothing
  sFinalizer       = Nothing

-- | @16kHz@
pattern DEFAULT_SAMPLE_RATE :: Double
pattern DEFAULT_SAMPLE_RATE = 16000

-- | 'MONO'
pattern DEFAULT_CHANNEL_COUNT :: Natural
pattern DEFAULT_CHANNEL_COUNT = MONO

-- | @1@
pattern MONO :: Natural
pattern MONO = 1

-- | @2@
pattern STEREO :: Natural
pattern STEREO = 2

-- | @Nothing@
pattern DEFAULT_INPUT_DEVICE :: InputDevice
pattern DEFAULT_INPUT_DEVICE = Nothing

{-|

@
 'spChannelCount'     = 'DEFAULT_CHANNEL_COUNT'
 'spSuggestedLatency' = 0.0
@

-}
defaultStreamParameters :: PaDeviceIndex -> StreamParameters i
defaultStreamParameters spDevice = StreamParameters{..}
 where
 spChannelCount     = fromIntegral DEFAULT_CHANNEL_COUNT
 spSuggestedLatency = PaTime 0.1 -- TODO units? if 0?

--old {{ StreamFormat i => }} Unnecessary, and harmed inference

--------------------------------------------------------------------------------
