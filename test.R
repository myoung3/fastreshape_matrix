#warning: this will take more than 32gb of ram to test
#appox 63 seconds vs 22.5 seconds on my machine.

source("fastreshape_matrix.R")
x <- matrix(1:1e9,ncol=1e5)

rnames <- rep(letters,length=nrow(x))
cnames <- rep(LETTERS,length=ncol(x))

system.time(a <- reshape_matrix(x,rnames,cnames))
system.time(b <- fast_reshape_matrix(x,rnames,cnames))
system.time(melt(x))

stopifnot(all.equal(a,b ))
