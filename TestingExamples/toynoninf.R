##This is example 1 in Albert et al.

y <- 0.1 o

n.sample <- 200
eps.init <- 2
iter.max <- 50000
v <- 0.001
beta <- 2
resample <- 3*n.sample
delta <- 0.1
method <- "noninformative"
adaptjump <- TRUE
summarystats <- FALSE
f.summarystats <- NULL

##prior is uniform on [-10,10]
d.prior <- function(par)
    return(ifelse( -10 <= par && par <= 10 , 0.05 , 0))

r.prior <- function()
    return(( runif(1)-0.5)*20)

##likelihood is sum of two normal random variables
r.model <- function(par)
    return( rnorm( 1 , 2*par , 1+0.01 ) )

##run SABC-fork
source("../SABC-fork/SABC.R")
source("../SABC-fork/SABC.noninf.r")
SABCfork.res <- SABC(
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
  f.summarystats = f.summarystats)

##run SABC
source("../SABC/SABC/R/SABC.R")
source("../SABC/SABC/R/SABC.noninf.r")
SABC.res <- SABC(
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
  f.summarystats = f.summarystats)

##draw everything
hist(SABCfork.res$E,prob=TRUE,breaks=20,xlim=c(-5,5),col=rgb(0,0,1,1/4))
hist(SABC.res$E,prob=TRUE,breaks=20,xlim=c(-5,5),col=rgb(1,0,0,1/4),add=TRUE)
legend("topright",legend=c("Fork","Original"),fill=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))
