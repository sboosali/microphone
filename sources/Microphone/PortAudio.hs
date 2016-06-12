{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, PatternSynonyms #-}
{-| an idiomatic wrapper around 'Sound.PortAudio', providing:

* parameter records (like @OpenProcess@)
* 'EitherT', for failability
* 'Managed', for resource management

-}
module Microphone.PortAudio where
import Microphone.Extra
import Microphone.Types()

import Sound.PortAudio
--import Sound.PortAudio.Base
--import Control.Monad.Managed
import Control.Monad.Trans.Either
import Control.Monad.Log
-- import Control.Error

import Control.Exception (throwIO)
-- import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Error.Class hiding (Error)

{-| -}

{-|

TODO ReaderT IsFatal

-}
newtype PortAudio' a = PortAudio { getPortAudio ::
 EitherT Error (LoggingT Warnings IO) a
-- LoggingT Warnings (EitherT Error IO) a
 } deriving
  ( Functor, Applicative, Monad
  , MonadIO
  , MonadError Error
--  , MonadLog Warnings
  )

{-old

{-| -}
type PortAudio' = PortAudioT IO

{-| -}
newtype PortAudioT m a = PortAudio { getPortAudio ::
-- EitherT Error (LoggingT Warnings IO) a
 LoggingT Warnings (EitherT Error m) a
 } deriving
  ( MonadLogging -- MonadEither
  , MonadIO
  )

-}

{-| -}
data Warnings = Warnings { getWarnings :: [Error] }

type IsFatal = Error -> Bool

--old
-- pattern WError :: Warnings
-- pattern WError = Warnings []

{-| Don't terminate the computation on nonfatal errors.
Instead, return unit.

If all errors are fatal, this has no effect:

@
suppressErrors 'allFatal' === id
@

If no errors are fatal, the computation never fails:

@
-- `m` is always performed
suppressErrors 'noneFatal' >> m
@

-}
suppressErrors :: IsFatal -> PortAudio' a -> PortAudio' ()
suppressErrors isFatal = alaPortAudio go
  where
  -- eitherT onFailure onSuccess :: EitherT e m a -> m (Either e ())
  go = eitherT onFailure onSuccess >>> EitherT
  -- ignore non-fatal errors

  onFailure e = if isFatal e
    then return (Left e)
    else return OK -- TODO log them

  -- ignore the result
  onSuccess _ = return OK

--old   onFailure e = if isFatal e then throwError e else return ()

alaPortAudio
 :: (EitherT Error (LoggingT Warnings IO) a -> EitherT Error (LoggingT Warnings IO) b) -> (PortAudio' a -> PortAudio' b)
alaPortAudio f = getPortAudio >>> f >>> PortAudio

-- | @= 'const' True@
allFatal :: IsFatal
allFatal = const True

-- | @= 'const' False@
noneFatal :: IsFatal
noneFatal = const False

-- | @Right ()@
pattern OK :: Either e ()
pattern OK = Right ()

-- deferErrors :: IsFatal
-- deferErrors = const False

{-| Eliminate an audio action.

Provide handlers for any warnings and for the error.

-}
runPortAudio
  :: IsFatal
  -> Handler IO Warnings
  -> (Either Error a -> IO b)
  -> PortAudio' a
  -> IO b
runPortAudio isError useWarnings useErrors
    = getPortAudio
 >>> runEitherT
 >>> (>>= (useErrors>>>liftIO))
 >>> (runLoggingT&flip) (useWarnings)

{-| For testing: print warnings and throw the error.
-}
runPortAudioSimple :: PortAudio' a -> IO a
runPortAudioSimple = runPortAudio
 allFatal
 (getWarnings >>> traverse_ print)
 (either throwIO return)

{-| Recover the original @portaudio@ API: ignore warnings and pass the error through.
-}
runPortAudioIgnoring :: PortAudio' a -> IO (Either Error a)
runPortAudioIgnoring = runPortAudio
 allFatal
 (const . return $ ())
 return
