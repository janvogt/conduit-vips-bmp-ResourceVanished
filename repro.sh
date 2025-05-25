

# Setting expectations, to prove that using vips on the command line works.
vips copy stdin .png < sample1.bmp > sample1.png
vips copy stdin .png < sample2.heic > sample2.png

# Triggering the issue.
runhaskell Repro.hs