module Repro where

import Conduit
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception
import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Unlift (unliftIO, withUnliftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Conduit.Process.Typed (ExitCode (..))
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Streaming.Process
import Data.Streaming.Process.Internal
import Debug.Trace qualified as Debug
import GHC.IO.Device (IODeviceType (..))
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.Internals (addHandleFinalizer, hClose_impl, handleFinalizer, mkDuplexHandleNoFinalizer, mkFileHandleNoFinalizer)
import System.IO
import System.IO qualified as IO
import System.IO.Error (ioeGetErrorType, isResourceVanishedErrorType)

main :: IO ()
main = do
  -- Consuming all input works fine.
  expect_to_produce cat "blob" "blob"
  -- If the process does not consume all input it fails with an exception, even
  -- if the process succeeds:
  -- Exception: fd:83: hClose: resource vanished (Broken pipe)
  expect_to_produce head "blob" "blob.head"
  where
    cat = typedProcess "cat" []
    head = typedProcess "head" ["-c", "10000"]
    expect_to_produce process source_file expectation_file = do
      result <- withSourceFile' source_file (`process` sinkLazy)
      expectation <- BL.readFile expectation_file
      unless (result == expectation) $
        putStrLn "This should not happen. Did you run via ./repro.sh?"

typedProcess ::
  FilePath ->
  [String] ->
  ConduitT () ByteString IO () ->
  ConduitT ByteString Void IO b ->
  IO b
typedProcess = process sourceProcessWithStreams'

process ::
  ( CreateProcess ->
    ConduitT () ByteString IO () ->
    ConduitT ByteString Void IO b ->
    ConduitT ByteString o IO BL.ByteString ->
    IO (ExitCode, b, BL.ByteString)
  ) ->
  FilePath ->
  [String] ->
  ConduitT () ByteString IO () ->
  ConduitT ByteString Void IO b ->
  IO b
process process_fn name args stdin stdout = do
  (exit_code, result, error_output) <-
    process_fn
      (proc name args)
      stdin
      stdout
      sinkLazy
  unless (BL.null error_output) $
    putStrLn $
      "Error Output: " ++ show (BL.toStrict error_output)
  unless (exit_code == ExitSuccess) $
    putStrLn $
      "Error Code: " ++ show exit_code
  return result

-- from Data.Conduit.Process.Typed
newtype ConduitInput' i o m n r r' = ConduitInput' (ConduitM i o m r, n r')

instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, i ~ ByteString) => InputSource (ConduitInput' i o m n r r') where
  isStdStream = (\(Just h) -> hSetBuffering h NoBuffering $> ConduitInput' (sinkHandle' h, liftIO $ hClose h), Just CreatePipe)

sourceProcessWithStreams' ::
  (MonadUnliftIO m) =>
  CreateProcess ->
  -- | stdin
  ConduitT () ByteString m () ->
  -- | stdout
  ConduitT ByteString Void m a ->
  -- | stderr
  ConduitT ByteString Void m b ->
  m (ExitCode, a, b)
sourceProcessWithStreams' cp producerStdin consumerStdout consumerStderr =
  withUnliftIO $ \u -> do
    ( ConduitInput' (sinkStdin, closeStdin),
      (sourceStdout, closeStdout),
      (sourceStderr, closeStderr),
      sph
      ) <-
      streamingProcess cp
    (_, resStdout, resStderr) <-
      runConcurrently
        ( (,,)
            <$> Concurrently
              ( (unliftIO u (runConduit $ producerStdin .| sinkStdin) `finally` closeStdin `catch` handle_vanished)
              )
            <*> Concurrently (unliftIO u $ runConduit $ sourceStdout .| consumerStdout)
            <*> Concurrently (unliftIO u $ runConduit $ sourceStderr .| consumerStderr)
        )
        `finally` (closeStdout >> closeStderr)
        `onException` terminateStreamingProcess sph
    ec <- waitForStreamingProcess sph
    return (ec, resStdout, resStderr)
  where
    handle_vanished (e :: IOError) =
      if isResourceVanishedErrorType (ioeGetErrorType e) then pure () else throwIO e

terminateStreamingProcess :: (MonadIO m) => StreamingProcessHandle -> m ()
terminateStreamingProcess = liftIO . terminateProcess . streamingProcessHandleRaw

-- from Conduit
withSourceFile' ::
  (MonadUnliftIO m, MonadIO n) =>
  FilePath ->
  (ConduitM i ByteString n () -> m a) ->
  m a
withSourceFile' fp inner =
  withRunInIO $ \run ->
    withBinaryFile' fp IO.ReadMode $
      run . inner . sourceHandle'

sourceHandle' ::
  (MonadIO m) =>
  IO.Handle ->
  ConduitT i S.ByteString m ()
sourceHandle' h =
  loop 0
  where
    loop n = do
      bs <- liftIO (S.hGetSome h 4)
      Debug.traceM $ "<loop " <> show n <> ">" <> show bs
      if S.null bs
        then return ()
        else yield bs >> loop (n + 1)

sinkHandle' ::
  (MonadIO m) =>
  IO.Handle ->
  ConduitT S.ByteString o m ()
sinkHandle' h = awaitForever (liftIO . S.hPut h)

-- from GHC.Internal.IO.Handle.FD

withBinaryFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' filepath iomode act =
  -- first open the file to get an FD
  FD.openFileWith
    filepath
    iomode
    True
    ( \fd fd_type -> do
        -- Then use it to make a Handle. If this fails, openFileWith
        -- will take care of closing the file.
        mkHandleFromFDNoFinalizer
          fd
          fd_type
          filepath
          iomode
          False {- do not *set* non-blocking mode -}
          Nothing
    )
    -- Add a finalizer to the handle. This is done under a mask,
    -- so there are no asynchronous exceptions, and (satisfying
    -- the conditions of openFileWith), addHandleFinalizer
    -- cannot throw a synchronous exception.
    ( \restore hndl -> do
        addHandleFinalizer hndl handleFinalizer
        r <- restore (act hndl) `onException` hClose_impl hndl
        hClose_impl hndl
        pure r
    )

mkHandleFromFDNoFinalizer ::
  FD.FD ->
  IODeviceType ->
  FilePath -> -- a string describing this file descriptor (e.g. the filename)
  IOMode ->
  Bool -> --  *set* non-blocking mode on the FD
  Maybe TextEncoding ->
  IO Handle
mkHandleFromFDNoFinalizer fd0 fd_type filepath iomode set_non_blocking mb_codec =
  do
    let _ = set_non_blocking -- warning suppression
    fd <- return fd0

    let nl
          | isJust mb_codec = nativeNewlineMode
          | otherwise = noNewlineTranslation

    mkFileHandleNoFinalizer fd filepath iomode mb_codec nl