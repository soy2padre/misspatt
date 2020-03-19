# Simulates missing data patterns for arbitrary data set
# n = N
# f = # of factors
# ipc = Indicators / factor
# c = Interfactor Correlation?
# r = Reliability?
# pmiss = Pr(Missing)
if(!require('MASS')) install.packages('MASS', dependencies=TRUE)
simdata <- function(n=1000, f=3, ipc=3, c=.4, r=.5, pmiss=.5) {
    ly <- matrix(rep(0, f*ipc), nrow=f*ipc, ncol=f)
    idx <- 1
    for (j in 1:f) {
        ly[idx:(idx+ipc-1), j] <- 1
        idx <- idx + ipc
    }
    ps <- matrix(rep(c,(f*f)), nrow=f, ncol=f) + diag((1-c), nrow=f, ncol=f)
    te = diag(((1 - r)/r), nrow=(f*ipc), ncol=(f*ipc))
    sigma <- ly %*% ps %*% t(ly) + te
    sigma <- cov2cor(sigma)
    data <- as.data.frame(mvrnorm(n=n, mu=rep(0, dim(sigma)[1]), Sigma=sigma) < qnorm(pmiss))
    return(data)
}
