# Creates correlation matrices from raw observed/missing data
# Outputs a [[list]] containing:
# J = # of variables
# n = N
# cmat = Pearson's correlation matrix
# rmat = Polychoric correlation matrix
if(!require('psych')) install.packages('psych', dependencies=TRUE)
mkmats <- function(x) {
    J <- dim(x)[2]
    n <- dim(x)[1]
    cmat <- cor(x)
    rmat <- polychoric(x)$rho
    output <- list(J, n, cmat, rmat)
    names(output) <- c('J', 'n', 'cmat', 'rmat')
    return(output)
}