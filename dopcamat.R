if(!require('paran')) install.packages('paran', dependencies=TRUE)
dopcamat <- function(n, mat, iters=1000, cent=95, ptype=FALSE) {
    out <- paran(n=n, mat=mat, iterations=iters, centile=cent, cfa=ptype, quietly=T, status=F)
    ev <- out$Ev
    ev[ev < 0] <- 0
    nparan <- out$Retained
    nkaiser <- kaiser(ev)
    njolliffe <- jolliffe(ev)
    nproflik <- proflik(ev)
    nekc <- unlist(ekc(J=dim(mat)[2], n=n, loadings=ev)[2])
    output <- cbind(nparan, nkaiser, njolliffe, nproflik, nekc)
    return(output)
}