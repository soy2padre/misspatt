dopca <- function(x, n, iters=1000, cent=95) {
    out <- paran(x, n, iterations=iters, centile=cent, quietly=T, status=F)
    ev <- out$Ev
    ev[ev < 0] <- 0
    nparan <- out$Retained
    nkaiser <- kaiser(ev)
    njolliffe <- jolliffe(ev)
    nproflik <- proflik(ev)
    nekc <- unlist(ekc(J=dim(x)[2], n=dim(x)[1], loadings=ev)[2])
    output <- cbind(nparan, nkaiser, njolliffe, nproflik, nekc)
    #names(output) <- c("nparan", "nkaiser", "njolliffe", "nproflik", "nekc")
    return(output)
}