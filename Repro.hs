module Repro where

import Conduit
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Process
import Data.Conduit.Process.Typed (ExitCode (..))

main :: IO ()
main = do
  expect_to_produce vips "sample2.heic" "sample2.png"
  expect_to_produce cat "sample2.heic" "sample2.heic"
  expect_to_produce cat "sample1.bmp" "sample1.bmp"
  -- The following fails with
  -- Repro.hs: sample1.bmp: withBinaryFile: resource vanished (Broken pipe)
  -- , despite vips the same call on the command line works.
  expect_to_produce vips "sample1.bmp" "sample1.png"
  where
    vips = streamProcess "vips" ["copy", "stdin", ".png"]
    cat = streamProcess "cat" []
    expect_to_produce process source_file expectation_file = do
      result <- withSourceFile source_file (`process` sinkLazy)
      expectation <- BL.readFile expectation_file
      unless (result == expectation) $
        putStrLn "This should not happen. Did you use the flake pinned version for all dependencies?"

streamProcess ::
  FilePath ->
  [String] ->
  ConduitT () ByteString IO () ->
  ConduitT ByteString Void IO b ->
  IO b
streamProcess name args stdin stdout = do
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