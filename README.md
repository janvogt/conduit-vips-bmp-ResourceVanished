# Minimal repduction for interaction between .bmp image, vips, and conduit-extra

Running `vips` on a `.bmp` file via Data.Conduit.Process.sourceProcessWithStreams
fails with

> Repro.hs: sample1.bmp: withBinaryFile: resource vanished (Broken pipe)

**However, the file `sample1.bmp` has certainly not vanished.**

Reproduce yourself by running `./repro.sh` within the provided environment.

## Environment

This reproduction uses nix, to ensure reproducability.

Run `nix develop` within this project or `direnv allow` to use `direnv`.

If you do not use nix, provide an environment with the following dependencies:

- vips 8.15.5
- ghc 9.6.6
- conduit 1.3.6
- conduit-extra 1.3.6

## Root cause analysis so far

### It's not vips

Runing the failing `vips` command `vips copy stdin .png < sample1.bmp > sample1.png`
via command line, it works. `./repro.sh` uses it to create the expectations.

Also the call to `vips` works for other image formats just fine, as shown by
the `sample2.heic` conversion.

### It's not the particular .bmp or it's filename

I've tested multiple `.bmp` files and varied their names.

### It's not `Data.Conduit.Process.sourceProcessWithStreams`

Using the same setup to run `cat` everything is fine.
