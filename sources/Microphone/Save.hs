{-|

TODO

LINEAR16
Uncompressed 16-bit signed little-endian samples.
putInt16le

FLAC

StreamFormat Int8
StreamFormat Int16
StreamFormat Int32
StreamFormat CFloat

putInt8le
putInt16le
putInt32le
putFloatle

-}
module Microphone.Save where
import Microphone.Extra
--import Microphone.Types

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

import Data.Int
import System.IO (openBinaryFile,IOMode(..),hClose)

-- saveAudio :: (StreamFormat i) => FilePath -> [i] -> IO () --TODO (StreamFormat i) => typecase
-- saveAudio path audio = do
--   saveBinary path audio_bytes

saveAudio_LINEAR16 :: FilePath -> [Int16] -> IO ()
saveAudio_LINEAR16 path shorts = do
 let bytes = encodeInt16s shorts
 saveBinary path bytes

-- | a 'BL.ByteString' is a packed list of bytes (@[Word8]@).
saveBinary :: FilePath -> BL.ByteString -> IO ()
saveBinary path bytes = do

  -- -- for idempotency
  -- mktree audioDir
  -- touch path

  h <- openBinaryFile path WriteMode
  () <- BL.hPut h bytes
  hClose h

encodeInt16s :: (Traversable t) => t Int16 -> BL.ByteString
encodeInt16s = traverse_ putInt16le >>> runPut
