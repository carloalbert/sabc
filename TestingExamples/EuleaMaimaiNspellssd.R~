                                        # Tobia: Parallel simulation for changing length of the Maimai time series.
n <- 500
Ktrue <- -0.1
Sigmatrue <- 0.5

source("../SABC/R/SABC.R")
source("../SABC/R/SABC.noninf.r")
source("../SABC/R/SABC.inf.r")

mydate=seq(from=as.Date("2014-1-1"),by="day",length.out=n)
library(MASS)
library(hydrostats)
library(tensorA)

## limits of the uniform distribution for prior
Kmin <- -2
Kmax <- 0.5
Sigmamin <- 0.01
Sigmamax <- 5

r.prior <- function(){
    K    <- runif(1,Kmin,Kmax)
    sigma<- runif(1,Sigmamin,Sigmamax)
    return(c(K,sigma))
}

rain <- as.matrix( read.table("Maimai240h.dat",skip=7,header=FALSE))[1:n,10]
true <- as.matrix( read.table("Maimai240h.dat",skip=7,header=FALSE))[1:n,9]
S0 <- 0.1

## Do an implicit euler scheme for solving the ODE
Eulea <- function(par){
    K <- 1/exp(par[1])
    out <- rep(0,n)
    out[1] <- S0
    for ( i in 2:n){
        out[i] <- (rain[i]+out[i-1])/(1+K)
    }
    return(out)
}



f.summarystats <-  function(x){
    myflow <- ts.format(x=data.frame(Date=mydate,Q=x),format="%Y-%m-%d")
    h <- sd(x[which(x<quantile(x,0.15))])
    f <- high.spells(myflow, quant=0.98, ann.stats=FALSE, plot=FALSE)$avg.spell.peak
    return(unname(unlist(c(f,h))))
}

## Call SABC  and plot posteriors
d.prior <- function(theta){
    ifelse(theta[1]<Kmax  && theta[1] > Kmin && theta[2]<Sigmamax  && theta[2] > Sigmamin, 1, 0 )
}


n.sample  <- 500           # Size of the ensemble
eps.init  <- 10          # start value for the tolerance (initial temperature)
iter.max  <- 40*n.sample   # number of draws from the likelihood we're willing to invest
v         <- 2         # Tuning parameter governing the annealing speed
beta      <- 0.8           # Tuning parameter governing mixing in parameter space
resample  <- 2000 #n.sample    # Number of succesful iterations after which a resampling is performed
delta     <- 0.1           # Tuning parameter for the resampling steps
adaptjump    <- TRUE               # jump distribution is adapted
summarystats <- FALSE               # Semi-automatic summary stats are used

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

r.model<- function(theta){
    sigma <- theta[2]
    y.det <-Eulea(theta)
    ysim <- y.det+rnorm(n,0,sigma)
    return(ysim)
}


## Do a synthetic study. This is the "observation"
y <- r.model(c(Ktrue,Sigmatrue))
truevalue <- f.summarystats(y)



method       <- "noninformative"

try(resnon <- SABC(
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
))

hist(resnon$E[,1],breaks=20)
hist(resnon$E[,2],breaks=20)


method       <- "informative"

try(resnon <- SABC(
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
))



