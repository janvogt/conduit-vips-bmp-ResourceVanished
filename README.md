# sourceProcessWithStreams fails if process does not consume all input

Reproduce by running `./repro.sh` within the provided environment.

## Environment

This reproduction uses nix, to ensure reproducability.

Run `nix develop` within this project or `direnv allow` to use `.envrc`.

If you do not use nix, provide an environment with the following dependencies:

- ghc 9.6.6
- conduit 1.3.6
- conduit-extra 1.3.6

## Expected behavior

Even if a process does not consume all input, the output and exit code should
be observable.

## Actual behaviour

If a process closes it's input handle, sourceProcessWithStreams crashes.

## Investigating

use `ghcid Repro.hs --run=":! ./repro.sh""` for good DX
