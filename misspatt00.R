# Tabula Rasa
rm(list = ls())
mydir <- getwd()
#setwd('./misspatt')

# Start Timing
start <- proc.time()

# Dependencies
if (!require('doRNG'))
  install.packages('doRNG', dependencies = TRUE)
if (!require('doParallel'))
  install.packages('doParallel', dependencies = TRUE)
if (!require('data.table'))
  install.packages('data.table', dependencies = TRUE)
if (!require('foreach'))
  install.packages('foreach', dependencies = TRUE)

# Parallel Setup
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Load Functions
source('simdata.R')
source('mkmats.R')
source('dopcamat.R')
source('Kaiser_Jolliffe_Proflik.R')
source('EKC.R')

# Simulation Parameters
reps <- 2
c <- .3
r <- .7

samp <- c(100, 250, 1000)
nf <- c(1, 3, 5, 10)
nipc <- c(3, 5, 10)
pctmiss <- c(.1, .25, .5)
ls <- length(samp)
lf <- length(nf)
li <- length(nipc)
lm <- length(pctmiss)
totlen <- reps * ls * lf * li * lm

# Specify that these carry across loops
pkgs <- c('paran', 'psych', 'MASS', 'parallel')

# Initialize output matrix
myx <- matrix(nrow = 0, ncol = 11)

# Establish seed for replication
rng <- RNGseq(totlen, 87658653)

# Run simulation loop
misspatt <- foreach(pm = 1:lm, .combine = rbind) %:%
  foreach(n = 1:ls, .combine = rbind) %:%
  foreach(f = 1:lf, .combine = rbind) %:%
  foreach(ipc = 1:li, .combine = rbind) %:%
  foreach(
    iter = 1:reps,
    .combine = rbind,
    .packages = pkgs,
    .inorder = FALSE,
    .errorhandling = "remove"
  ) %dopar% {
    # Make this many sets of seeds
    k <- (pm - 1) * ls * lf * li * reps +
      (n - 1) * lf * li * reps +
      (f - 1) * li * reps +
      (ipc - 1) * reps + iter
    
    # Seed for _this_ iteration
    rngtools::setRNG(rng[[k]])
    
    # Create data matrix
    mydata <-
      simdata(
        n = samp[n],
        f = nf[f],
        ipc = nipc[ipc],
        c = c,
        r = r,
        pmiss = pctmiss[pm]
      )
    
    # Convert it to matrix form
    mymats <- mkmats(mydata)
    
    # Run PCA on correlation and polychoric matrices with extraction criteria
    myout <- rbind(dopcamat(n = mymats$n, mat = mymats$cmat, ptype=FALSE),
                   dopcamat(n = mymats$n, mat = mymats$rmat, ptype=FALSE),
                   dopcamat(n = mymats$n, mat = mymats$cmat, ptype=TRUE),
                   dopcamat(n = mymats$n, mat = mymats$rmat, ptype=TRUE))
    cond <-
      rbind(
        cbind(iter, samp[n], f = nf[f], ipc = nipc[ipc], pctmiss[pm], 'pearson', 'pca'),
        cbind(iter, samp[n], f = nf[f], ipc = nipc[ipc], pctmiss[pm], 'tetrachoric', 'pca'),
        cbind(iter, samp[n], f = nf[f], ipc = nipc[ipc], pctmiss[pm], 'pearson', 'paf'),
        cbind(iter, samp[n], f = nf[f], ipc = nipc[ipc], pctmiss[pm], 'tetrachoric', 'paf')
      )
    colnames(cond)[5] <- 'pmiss'
    colnames(cond)[6] <- 'corrtype'
    colnames(cond)[7] <- 'ptype'
    cbind(cond, myout)
  }

# Convert and save results
#misspatt <- as.data.table(misspatt)
#save(misspatt, file = 'misspatt00.Rdata')

# Stop cluster
stopImplicitCluster()
stopCluster(cl)

# Stop and return timer
stop <- proc.time()
time <- stop - start
time
