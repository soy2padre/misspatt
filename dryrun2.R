rm(list=ls())
start <- proc.time()
set.seed(72855)
if(!require('doParallel')) install.packages('doParallel', dependencies=TRUE)
if(!require('foreach')) install.packages('foreach', dependencies=TRUE)
ncores <- detectCores()
registerDoParallel(cores=ncores)
#setwd('/Volumes/TUWORK/Documents/Missing_Data')
#setwd('~/Missing_Data')
source('simdata.R')
source('mkmats.R')
source('dopcamat.R')
source('Kaiser_Jolliffe_Proflik.R')
source('EKC.R')

reps <- 5
c <- .4
r <- .5

# samp <- c(100, 250, 1000)
# nf <- c(3, 5, 10)
# nipc <- c(3, 5, 10)
# pctmiss <- c(.1, .25, .5)
samp <- c(100)
nf <- c(3)
nipc <- c(3)
pctmiss <- c(.1)

bigx <- matrix(nrow=0, ncol=11)
for (n in samp) {
    for (f in nf) {
        for (ipc in nipc) {
            for (pmiss in pctmiss) {
                bigout <- matrix(nrow=0, ncol=11)
                myx <- foreach(iter=1:reps, .combine=rbind) %dopar% {
                  
                    # Generate Data
                    mydata <- simdata(n=n, f=f, ipc=ipc, c=c, r=r, pmiss=pmiss)
                    
                    # Generate Matrices
                    mymats <- mkmats(mydata)
                    
                    # Generate and Process Eigenvalues
                    tryCatch({
                    myout <- rbind(dopcamat(n=mymats$n, mat=mymats$cmat),
                                   dopcamat(n=mymats$n, mat=mymats$rmat))
                    cond <- rbind(cbind(iter, n, f, ipc, pmiss, 'pearson'),
                                  cbind(iter, n, f, ipc, pmiss, 'tetrachoric'))
                    colnames(cond)[6] <- 'corrtype'
                    myout <- cbind(cond,myout)
                    },
                    finally={
                    # Handle Output
                    bigout <- rbind(bigout, myout)
                    })
                }
                bigx <- rbind(bigx, myx)
            }
        }
    }
}

save(bigx, file='bigx.Rdata')
stop <- proc.time()
time <- stop - start
time
