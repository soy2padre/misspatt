ekc <- function(J=20, n=200, loadings=l) {
    loadings <- c(0, loadings)
    lekc <- as.vector(rep(NA,J+1))
    gamma <- J / n
    lup <- (1 + sqrt(gamma))^2
    lekc[1] <- 0
    for (j in 1:(J+1)) {
        lekc[j] <- max(1,lup*((J - sum(loadings[1:(j-1)]))/(J - (j-1) + 1)))
    }
    lekc <- lekc[2:(J+1)]
    loadings <- loadings[2:(J+1)]
    nfact <- sum(loadings>=lekc)
    output <- list(lekc, nfact)
    return(output)
}
