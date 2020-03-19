kaiser <- function(eigs) {
    sum(eigs >= 1)
}

jolliffe <- function(eigs) {
    sum(eigs >= .7)
}

proflik <- function(x)
{
    
    myvar <- function(x)
    {
        if (length(x) == 1) 
            return (0)
        else 
            return(var(x))
    }
    
    x <- sort(x, decr=T);
    n <- length(x);
    lik <- rep(0, n);
    for (i in 1:(n-1))
    {
        u <- mean(x[1:i]); s1 <- myvar(x[1:i]);
        v <- mean(x[(i+1):n]); s2 <- myvar(x[(i+1):n]);
        s <- sqrt(((i-1)*s1+(n-i-1)*s2)/(n-2))
        lik[i] <- sum(dnorm(x[1:i], u, s, log=T))+sum(dnorm(x[(i+1):n], v, s, log=T)) 
    }
    
    u <- mean(x); s <- sqrt(var(x));
    lik[n] <- sum(dnorm(x, u, s, log=T)) 
    
    top<-which(lik==max(lik));
    return(top)
}