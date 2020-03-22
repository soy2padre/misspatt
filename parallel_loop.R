rm(list=ls())
start <- proc.time()

if(!require('doRNG')) install.packages('doRNG', dependencies=TRUE)
if(!require('doParallel')) install.packages('doParallel', dependencies=TRUE)
#if(!require('parallel')) install.packages('parallel', dependencies=TRUE)
if(!require('foreach')) install.packages('foreach', dependencies=TRUE)
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
registerDoParallel(cl)
iters <- 100
as <- c(1, 2)
bs <- c(3, 4)
cs <- c(5, 6)
ds <- c(7, 8)
pkgs <- c('paran', 'psych', 'MASS', 'parallel')

rng <- RNGseq(iters*length(ds), 87658653)

#mcoptions <- list(preschedule=FALSE, mc.set.seed=TRUE)

allv <- matrix(nrow=0, ncol=3)

# k <- 1
output <-
  foreach(d=1:length(ds), .combine=rbind) %:%
  foreach(iter=1:iters, .packages=pkgs, .combine=rbind, .inorder=FALSE) %dopar% {
            rngtools::setRNG(rng[[(d-1)*iters + iter]])
            # k <- k + 1
    cbind(iter, d, runif(n=1))
    # allv <- rbind(allv, v)
      }
#bigx <- rbind(bigx, myx)

stopCluster(cl)
stopImplicitCluster()
stop <- proc.time()
time <- stop - start
time
