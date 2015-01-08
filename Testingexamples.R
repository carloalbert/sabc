n <- 500
Ktrue <- -0.1
Sigmatrue <- 0.5
mydate=seq(from=as.Date("2014-1-1"),by="day",length.out=n)
source("SABC/R/SABC.R")
source("SABC/R/SABC.noninf.r")
source("SABC/R/SABC.inf.r")
library(MASS)
library(tensorA)
library(hydroTSM)
library(msm)
Kmin <- -2
Kmax <- 0.5
Sigmamin <- 0.01
Sigmamax <- 5

rain <- as.matrix( read.table("Maimai240h.dat",skip=7,header=FALSE))[1:n,10]
S0 <- 0.1

Eulea <- function(par){
    K <- 1/exp(par[1])
    out <- rep(0,n)
    out[1] <- S0
    for ( i in 2:n){
        out[i] <- (rain[i]+out[i-1])/(1+K)
    }
    return(out)
}

r.prior <- function(){
    K    <- runif(1,Kmin,Kmax)
    sigma<- runif(1,Sigmamin,Sigmamax)
    return(c(K,sigma))
}

d.prior <- function(theta){
    ifelse(theta[1]<Kmax  && theta[1] > Kmin &&
           theta[2]<Sigmamax  && theta[2] > Sigmamin, 1, 0 )
}
r.priorinf <- function(){
    K    <- rtnorm(1,Ktrue,0.3,Kmin,Kmax)
    sigma<- rtnorm(1,Sigmatrue,0.3,Sigmamin,Sigmamax)
    return(c(K,sigma))
}

d.priorinf <- function(theta){
    return(dtnorm(theta[1],Ktrue,0.3,Kmin,Kmax) * dtnorm(theta[2],Sigmatrue,0.3,Sigmamin,Sigmamax))
}

r.model<- function(theta){
    sigma <- theta[2]
    y.det <-Eulea(theta)
    ysim <- y.det+rnorm(n,0,sigma)
    return(ysim)
}

f.summarystats <- function(x){
    myx <- ifelse(x>0,x,0)
    a <- fdc(myx,log="y",plot=F)
    f <- min(myx[which(a<0.05)])
    h <-  max(myx[which(a>1-sum(myx==0)/n-0.05)])
    return(unname(unlist(c(f,h))))
}

r.modeldistance <- function(theta){
    if (theta[1]<Kmax && theta[1]>Kmin  && theta[2] > Sigmamin &&
        theta[2]<Sigmamax){
        sigma <- theta[2]
        y.det <-Eulea(theta)
        ysim <- y.det+rnorm(n,0,sigma)
        scores <-(f.summarystats(ysim)-truevalue)/truevalue
        return(max(abs(scores)))
    }else
        return(sqrt(sum(truevalue^2)))
}

n.sample  <- 500
eps.init  <- 10
iter.max  <- 80*n.sample
v         <- 2
beta      <- 0.8
resample  <- 2000
delta     <- 0.1
adaptjump    <- TRUE

y <- r.model(c(Ktrue,Sigmatrue))
truevalue <- f.summarystats(y)


## PART 1 noninformative without summarystats
summarystats <- FALSE
method       <- "noninformative"
res <- SABC(
            r.model       = r.modeldistance,
            r.prior       = r.prior,
            d.prior       = d.prior,
            n.sample      = n.sample,
            eps.init      = eps.init,
            iter.max      = iter.max,
            v             = v,
            beta          = beta,
            delta         = delta,
            resample      = resample,
            verbose       = n.sample,
            method        = method,
            adaptjump     = adaptjump,
            summarystats  = summarystats,
            y             = NULL,
            f.summarystats = FALSE
    )
par(mfrow=c(2,8))
hist(res$E[,1],breaks=50,xlim=c(-1,0.5),main="noninf,noSS,K")
hist(res$E[,2],breaks=50,xlim=c(0,1),,main="noninf,noSS,Sigma")



## PART 2 noninformative with summarystats
summarystats <- TRUE
method       <- "noninformative"
res <- SABC(
            r.model       = r.model,
            r.prior       = r.prior,
            d.prior       = d.prior,
            n.sample      = n.sample,
            eps.init      = eps.init,
            iter.max      = iter.max,
            v             = v,
            beta          = beta,
            delta         = delta,
            resample      = resample,
            verbose       = n.sample,
            method        = method,
            adaptjump     = adaptjump,
            summarystats  = summarystats,
            y             = y,
            f.summarystats = FALSE
    )
hist(res$E[,1],breaks=50,xlim=c(-1,0.5),main="noninf,yesSS,K")
hist(res$E[,2],breaks=50,xlim=c(0,1),,main="noninf,yesSS,Sigma")


## PART 3 informative without summarystats
summarystats <- FALSE
method       <- "informative"
res <- SABC(
            r.model       = r.modeldistance,
            r.prior       = r.priorinf,
            d.prior       = d.priorinf,
            n.sample      = n.sample,
            eps.init      = eps.init,
            iter.max      = iter.max,
            v             = v,
            beta          = beta,
            delta         = delta,
            resample      = resample,
            verbose       = n.sample,
            method        = method,
            adaptjump     = adaptjump,
            summarystats  = summarystats,
            y             = NULL,
            f.summarystats = FALSE
    )

hist(as.matrix(res$E)[,1],breaks=50,xlim=c(-1,0.5),main="inf,noSS,K")
hist(as.matrix(res$E)[,2],breaks=50,xlim=c(0,1),,main="inf,noSS,Sigma")


## PART 4 informative with summarystats

summarystats <- TRUE
method       <- "informative"
res <- SABC(
            r.model       = r.model,
            r.prior       = r.priorinf,
            d.prior       = d.priorinf,
            n.sample      = n.sample,
            eps.init      = eps.init,
            iter.max      = iter.max,
            v             = v,
            beta          = beta,
            delta         = delta,
            resample      = resample,
            verbose       = n.sample,
            method        = method,
            adaptjump     = adaptjump,
            summarystats  = summarystats,
            y             = y,
            f.summarystats = FALSE
    )

hist(as.matrix(res$E)[,1],breaks=50,xlim=c(-1,0.5),main="inf,noSS,K")
hist(as.matrix(res$E)[,2],breaks=50,xlim=c(0,1),,main="inf,noSS,Sigma")
