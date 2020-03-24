# Master code for running analyses
rm(list=ls())
start <- proc.time()

if(!require('doRNG')) install.packages('doRNG', dependencies=TRUE)
if(!require('doParallel')) install.packages('doParallel', dependencies=TRUE)
if(!require('parallel')) install.packages('parallel', dependencies=TRUE)
if(!require('foreach')) install.packages('foreach', dependencies=TRUE)
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
registerDoParallel(cl)
source('simdata.R')
source('mkmats.R')
source('dopcamat.R')
source('Kaiser_Jolliffe_Proflik.R')
source('EKC.R')

reps <- 100
c <- .4
r <- .5

samp <- c(100, 250, 1000)
nf <- c(3, 5, 10)
nipc <- c(3, 5, 10)
pctmiss <- c(.1, .25, .5)
ls <- length(samp)
lf <- length(nf)
li <- length(nipc)
lm <- length(pctmiss)
totlen <- reps*ls*lf*li*lm

pkgs <- c('paran', 'psych', 'MASS', 'parallel')

myx <- matrix(nrow=0, ncol=11)

rng <- RNGseq(totlen, 87658653)

#mcoptions <- list(preschedule=FALSE, mc.set.seed=TRUE)

myx <- foreach(pm=1:lm, .combine=rbind) %:%
  foreach(n=1:ls, .combine=rbind) %:%
  foreach(f=1:lf, .combine=rbind) %:%
  foreach(ipc=1:li, .combine=rbind) %:%
  foreach(iter=1:reps, .combine=rbind, .packages=pkgs, .inorder=FALSE, .errorhandling="remove") %dopar% {
    k <- (pm - 1)*ls*lf*li*reps + 
      (n - 1)*lf*li*reps +
      (f - 1)*li*reps +
      (ipc - 1)*reps + iter
    rngtools::setRNG(rng[[k]])
    mydata <- simdata(n=samp[n], f=nf[f], ipc=nipc[ipc], c=c, r=r, pmiss=pctmiss[pm])
    mymats <- mkmats(mydata)
    myout <- rbind(dopcamat(n=mymats$n, mat=mymats$cmat),
                   dopcamat(n=mymats$n, mat=mymats$rmat))
    cond <- rbind(cbind(iter, n, f=nf[f], ipc=nipc[ipc], pctmiss[pm], 'pearson'),
                  cbind(iter, n, f=nf[f], ipc=nipc[ipc], pctmiss[pm], 'tetrachoric'))
    colnames(cond)[5] <- 'pmiss'
    colnames(cond)[6] <- 'corrtype'
    cbind(cond,myout)
    #cbind(iter, nipc[ipc], nf[f], samp[n], pctmiss[pm])
  }

save(myx, file='bigx4.Rdata')
stopImplicitCluster()
stopCluster(cl)
stop <- proc.time()
time <- stop - start
time
