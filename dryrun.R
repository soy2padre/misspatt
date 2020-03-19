rm(list=ls())
#if(!require('doParallel')) install.packages('doParallel', dependencies=TRUE)
#ncores <- detectCores()
#cl <- makeCluster(ncores, type="SOCK")

#setwd('/Volumes/TUWORK/Documents/Missing_Data')
setwd('~/Missing_Data')
source('simdata.R')
source('mkmats.R')
source('dopcamat.R')
source('Kaiser_Jolliffe_Proflik.R')
source('EKC.R')

reps <- 1000
c <- .4
r <- .5
bigout <- matrix(nrow=0, ncol=11)

samp <- c(100, 250, 1000)
nf <- c(3, 5, 10)
nipc <- c(3, 5, 10)
pctmiss <- c(.1, .25, .5)
# samp <- 100
# nf <- 1
# nipc <- 3
# reps <- 2
# pctmiss <- .1

for (n in samp) {
    for (f in nf) {
        for (ipc in nipc) {
            for (pmiss in pctmiss) {
                for (iter in 1:reps) {
                  print(as.integer(c(n, f, ipc, (100*pmiss), iter)))
                    # Generate Data
                    mydata <- simdata(n=n, f=f, ipc=ipc, c=c, r=r, pmiss=pmiss)
                    
                    # Generate Matrices
                    mymats <- mkmats(mydata)
                    
                    # Generate and Process Eigenvalues
                    invisible(myout <- rbind(dopcamat(n=mymats$n, mat=mymats$cmat),
                                   dopcamat(n=mymats$n, mat=mymats$rmat)))
                    cond <- rbind(cbind(iter, n, f, ipc, pmiss, 'pearson'),
                                  cbind(iter, n, f, ipc, pmiss, 'tetrachoric'))
                    colnames(cond)[6] <- 'corrtype'
                    myout <- cbind(cond,myout)
                    
                    # Handle Output
                    bigout <- rbind(bigout, myout)
                }
            }
        }
    }
}
save('bigout1.Rdata')
