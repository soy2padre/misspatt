# MAP function
# Based on function by Brian P. O'Connor
# https://github.com/bpoconnor

# function(n, mat, iters=1000, cent=95)
map <- function (n, rdata) {
  nvars  <- ncol(rdata)
  eigval  <- diag(eigs$d)
  eigvect <- eigs$u
  
  rm(eigs)
  
  loadings <- eigvect %*% sqrt(eigval)
  
  fmfm4  <- matrix(NA,nvars,3)
  fmfm4[,1]  <- 0:(nvars-1)
  fmfm4[1,2] <- (sum(sum(rdata^2))-nvars)/(nvars*(nvars-1))
  fmfm4[1,3] <- (sum(sum(rdata^4))-nvars)/(nvars*(nvars-1))
  
  
  # pb <- utils::txtProgressBar(min = 0, max = (nvars - 1), style = 3) # create progress bar
  for (m in 1:(nvars - 1)) {
    #     Sys.sleep(0.1) # for the progress bar
    a <- loadings[,1:m]
    partcov <- as.matrix(rdata - tcrossprod(a,a))  # faster than as.matrix(rdata - (a %*% t(a)))
    
    if (max(partcov) > .0001) {
      d <- diag ( (1 / sqrt(diag(partcov))))
      pr <- d %*% (partcov %*% d)  # faster than d %*% partcov %*% d
      fmfm4[m+1,2] <- (sum(sum(pr^2))-nvars)/(nvars*(nvars-1))
      fmfm4[m+1,3] <- (sum(sum(pr^4))-nvars)/(nvars*(nvars-1))
    }	else {break}	
    
    #	rm(a,partcov,d,pr) # remove large matrices to free up memory re: R creates duplicates
    #     utils::setTxtProgressBar(pb, m) # update progress bar
  }
  # close(pb)
  
  
  # identifying the smallest fm values & their locations
  nfMAP   <- which.min(na.omit(fmfm4[,2])) - 1
  nfMAP4  <- which.min(na.omit(fmfm4[,3])) - 1
  
  dimnames(fmfm4) <-list(rep("", dim(fmfm4)[1]))
  colnames(fmfm4) <- c("root","  Avg.Corr.Sq.","  Avg.Corr.power4")
  
  evals <- cbind(1:nvars, (diag(eigval)))
  dimnames(evals) <-list(rep("", dim(evals)[1]))
  colnames(evals) <- c("root"," eigenvalue")
  