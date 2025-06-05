# Creating a blob that is bigger than and not a multiple of
# Data.ByteString.Lazy.defaultChunkSize.
dd if=/dev/random of=blob count=150 bs=1024 2>/dev/null
# Getting the first 10 bytes of the blob.
cat blob | head -c 10000  | dd of=blob.head 2>/dev/null

# Triggering the issue.
runhaskell -threaded -rtsopts -with-rtsopts=-N Repro.hs