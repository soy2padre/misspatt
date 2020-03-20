# Master code for running analyses
rm(list=ls())
start <- proc.time()
if(!require('doParallel')) install.packages('doParallel', dependencies=TRUE)
if(!require('parallel')) install.packages('parallel', dependencies=TRUE)
if(!require('foreach')) install.packages('foreach', dependencies=TRUE)
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
#clusterSetupRNG(cl, type='RNGstream', seed=c(304193, 937797, 840893, 650999, 679473, 35296))
registerDoParallel(cl)
#setwd('/Volumes/TUWORK/Documents/Missing_Data')
#setwd('~/Missing_Data')
source('simdata.R')
source('mkmats.R')
source('dopcamat.R')
source('Kaiser_Jolliffe_Proflik.R')
source('EKC.R')

reps <- 2
c <- .4
r <- .5

samp <- c(100, 250, 1000)
nf <- c(3, 5, 10)
nipc <- c(3, 5, 10)
pctmiss <- c(.1, .25, .5)
k <- reps*length(samp)*length(nf)*length(nipc)*length(pctmiss)

n <- c(100)
f <- c(3)
pkgs <- c('paran', 'psych', 'MASS', 'parallel')

bigx <- matrix(nrow=0, ncol=11)
bigout <- matrix(nrow=0, ncol=11)
myx <- matrix(nrow=0, ncol=11)

#RNGkind("L'Ecuyer-CMRG")
#set.seed(72855)
.Random.seed <- c("L'Ecuyer-CMRG", 1, 2, 3, 4, 5, 6)

mcoptions <- list(preschedule=FALSE, mc.set.seed=TRUE)

myx <- foreach(pmiss=pctmiss, .combine=rbind) %:%
  foreach(n=samp, .combine=rbind) %:%
  foreach(f=nf, .combine=rbind) %:%
  foreach(ipc=nipc, .combine=rbind) %:%
  foreach(iter=1:reps, .combine=rbind, .packages=pkgs, .inorder=FALSE, .options.multicore=mcoptions) %dopar% {
    print(iter)
    # Generate Data
    mydata <- simdata(n=n, f=f, ipc=ipc, c=c, r=r, pmiss=pmiss)
    
    # Generate Matrices
    mymats <- mkmats(mydata)
    
    # Generate and Process Eigenvalues
    myout <- rbind(dopcamat(n=mymats$n, mat=mymats$cmat),
                   dopcamat(n=mymats$n, mat=mymats$rmat))
    cond <- rbind(cbind(iter, n, f, ipc, pmiss, 'pearson'),
                  cbind(iter, n, f, ipc, pmiss, 'tetrachoric'))
    colnames(cond)[6] <- 'corrtype'
    myout <- cbind(cond,myout)
    # Handle Output
    bigout <- rbind(bigout, myout)
  }
bigx <- rbind(bigx, myx)

save(bigx, file='bigx3.Rdata')
#stopCluster(cl)
stopImplicitCluster()
stop <- proc.time()
time <- stop - start
time
