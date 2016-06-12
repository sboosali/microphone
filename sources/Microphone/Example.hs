{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Microphone.Example where
import Microphone()

import Sound.PortAudio

{-|
@
stack build && stack exec -- example-microphone
@
-}
main :: IO ()
main = do
 putStrLn "(Microphone.Example...)"
 withPortAudio $ do
     n <- getNumDevices -- >>= either (fail.show) return
     print n
     return $ Right ()
 return ()
