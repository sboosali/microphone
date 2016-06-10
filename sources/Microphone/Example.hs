{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Microphone.Example where
import Microphone()

{-|
@
stack build && stack exec -- example-microphone
@
-}
main :: IO ()
main = do
 putStrLn "(Microphone.Example...)"

