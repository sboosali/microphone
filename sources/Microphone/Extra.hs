module Microphone.Extra
 ( module Microphone.Extra
 , module X
 ) where

import Control.DeepSeq as X (NFData)
import Data.Hashable as X (Hashable)
import Data.Semigroup as X (Semigroup)

import GHC.Generics as X (Generic)
import Data.Data as X (Data)

import Control.Arrow as X ((>>>))
import Data.Function as X ((&))
import Data.Foldable as X (traverse_,toList)
import Data.Monoid as X ((<>))
import Control.Monad as X
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM,atomically)
import Control.Monad.IO.Class (MonadIO(..))

nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)

whileM :: Monad m => m Bool -> m () -> m ()
whileM mb m = loop
  where
  loop = do
    b <- mb
    if b
    then do
      m
      loop
    else
      nothing

-- whileJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
-- whileJustM =
--   mb m = loop
--   where
--   loop = do
--     b <- mb
--     if b
--     then do
--       m
--       loop
--     else
--       nothing

-- | pause the thread for some milliseconds
pause :: Int -> IO ()
pause = threadDelay . (*1000)

liftSTM :: (MonadIO m) => STM a -> m a
liftSTM = liftIO . atomically

-- -- |Return a lazy list representing the contents of the supplied
-- -- 'TChan', much like 'System.IO.hGetContents'.
-- getTChanContents :: TChan a -> IO [a]
-- getTChanContents ch
--   = unsafeInterleaveIO (atomically $ do
--         x  <- readTChan ch
--         xs <- getTChanContents ch
--         return (x:xs)
--     )

{-TODO You can't nest STM Transactions, so maybe unsafeInterleaveIO Will fail.

: TChan a -> Chan a

: TChan a -> IO [a]

-}
