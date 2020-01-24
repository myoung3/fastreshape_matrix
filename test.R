#warning: this will take more than 32gb of ram to test

source("fastreshape_matrix.R")
x <- matrix(1:1e9,ncol=1e5)

rnames <- rep(letters,length=nrow(x))
cnames <- rep(LETTERS,length=ncol(x))

system.time(a <- reshape_matrix(x,rnames,cnames))
system.time(b <- fast_reshape_matrix(x,rnames,cnames))
system.time(c <- melt(x)) 

#melt(x) is slightly faster, but this doesn't include the time it takes to get the returned variable columns 
 #from integers to their original representation as row and column names in character/factor

stopifnot(all.equal(a,b ))
