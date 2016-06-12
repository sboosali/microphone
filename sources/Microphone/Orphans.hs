{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO
module Microphone.Orphans where

import Sound.PortAudio (Error,StreamOpenFlag(..))
import Sound.PortAudio.Base (PaDeviceIndex(..))

import Control.DeepSeq  (NFData)
import Data.Hashable  (Hashable)

import GHC.Generics  (Generic)
-- TODO import Data.Data  (Data)
import Foreign.C.Types (CInt(..))
import Control.Exception (Exception)

-- TODO mv to pkg

deriving instance Read    PaDeviceIndex
deriving instance Ord     PaDeviceIndex
deriving instance Generic PaDeviceIndex
deriving instance Integral PaDeviceIndex
deriving instance Real PaDeviceIndex
instance      Hashable PaDeviceIndex
instance        NFData PaDeviceIndex

deriving instance Generic StreamOpenFlag
instance Hashable StreamOpenFlag
instance   NFData StreamOpenFlag

instance Exception Error

deriving instance Generic CInt --TODO Why not in base?
instance Hashable CInt

-- Set isn't algebraic, it's opaque
-- instance Hashable a => Hashable (Set a)

