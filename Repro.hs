module Repro where

import Conduit
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (SomeException (..), finally, onException, throw, try)
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

main :: IO ()
main = do
  -- Consuming all input works fine.
  expect_to_produce cat "blob" "blob"
  -- If the process does not consume all input it fails with an exception, even
  -- if the process succeeds:
  -- Repro.hs: blob: withBinaryFile: resource vanished (Broken pipe)
  expect_to_produce head "blob" "blob.head"
  where
    cat = run "cat" []
    head = run "head" ["-c", "10000"]
    expect_to_produce process source_file expectation_file = do
      result <- withSourceFile source_file (`process` sinkLazy)
      expectation <- BL.readFile expectation_file
      unless (result == expectation) $
        putStrLn "This should not happen. Did you run via ./repro.sh?"

run ::
  FilePath ->
  [String] ->
  ConduitT () ByteString IO () ->
  ConduitT ByteString Void IO b ->
  IO b
run name args stdin stdout = do
  (exit_code, result, error_output) <-
    sourceProcessWithStreams
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
