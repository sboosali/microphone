{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, PatternSynonyms #-}
{-| an idiomatic wrapper around 'Sound.PortAudio', providing:

* parameter records (like @OpenProcess@)
* 'EitherT', for failability
* 'Managed', for resource management

-}
module Microphone.PortAudio where
import Microphone.Extra
import Microphone.Types

import Sound.PortAudio
--import Sound.PortAudio.Base
--import Control.Monad.Managed
import Control.Monad.Trans.Either
--import Control.Monad.Log
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
 PortAudio'' a
-- EitherT Error (LoggingT Warnings IO) a
 } deriving
  ( Functor, Applicative, Monad
  , MonadIO
  , MonadError Error
--  , MonadLog Warnings
  )

type PortAudio'' = EitherT Error (IO)

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

{-| A list of nonfatal @portaudio@ errors
(that have been 'suppress'ed).

-}
newtype Warnings = Warnings { getWarnings ::
                                [Error]
                            }
 deriving (Show,Read,Eq,Ord,Monoid)

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
suppressErrors isFatal = alaPortAudio $ suppress isFatal (const nothing) -- TODO log them

--old   onFailure e = if isFatal e then throwError e else return ()

alaPortAudio
 :: (PortAudio'' a -> PortAudio'' b) -> (PortAudio' a -> PortAudio' b)
alaPortAudio f = getPortAudio >>> f >>> PortAudio

-- | @= 'const' True@
allFatal :: e -> Bool
allFatal = const True

-- | @= 'const' False@
noneFatal :: e -> Bool
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
--  -> Handler IO Warnings
  -> (Either Error a -> IO b)
  -> PortAudio' a
  -> IO b
-- runPortAudio isError useWarnings useErrors
runPortAudio _isError useErrors
    = getPortAudio
 >>> runEitherT
 >>> (>>= (useErrors>>>liftIO))

{-| For testing: print warnings and throw the error.
-}
runPortAudioSimple :: PortAudio' a -> IO a
runPortAudioSimple = runPortAudio
 allFatal
-- (getWarnings >>> traverse_ print)
 (either throwIO return)

{-| Recover the original @portaudio@ API: ignore warnings and pass the error through.
-}
runPortAudioIgnoring :: PortAudio' a -> IO (Either Error a)
runPortAudioIgnoring = runPortAudio
 allFatal
-- (const . return $ ())
 return

--------------------------------------------------------------------------------

{-| wraps 'withStream'

-}
withStream'
  :: (StreamFormat input, StreamFormat output)
 => OpenStream input output
 -> (Stream input output -> PortAudio a)
 -> PortAudio a

withStream' OpenStream{..}
  = withStream sInput sOutput sSampleRate sFramesPerBuffer sFlags sCallback sFinalizer

--------------------------------------------------------------------------------

{-| Don't abort the action on nonfatal errors.
Instead, return unit.

If all errors are fatal, this has no effect:

@
suppress 'allFatal' 'nothing' === id
@

If no errors are fatal, the computation never fails:

@
-- `m` is always performed
suppress 'noneFatal' 'nothing' >> m
@

-}
suppress
  :: (Monad m)
  => (e -> Bool)
  -> (e -> m ())
  -> EitherT e m a
  -> EitherT e m ()
suppress isFatal useError = go
  where

  -- eitherT onFailure onSuccess :: EitherT e m a -> m (Either e ())
  go = eitherT onFailure onSuccess >>> EitherT

  -- ignore non-fatal errors
  onFailure e = if isFatal e
    then return (Left e)
    else do
        useError e
        return OK

  -- ignore the result
  onSuccess _ = return OK

--------------------------------------------------------------------------------
