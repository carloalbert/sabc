# ====================================================
#
# SABC with entropy measurements
#
# ====================================================

library(FNN)

sigma1 <- .1
sigma2 <- .01
r.model <- function(theta)
{
  if (runif(1)<0.5) return(rnorm(1,theta,  sigma1))
  else              return(rnorm(1,theta/2,sigma2))
}

# exact posterior:

f.post <- function(theta)
{
  (exp(-( theta   - y )^2/( 2*sigma1^2 ))/sigma1 +
   exp(-( theta/2 - y )^2/( 2*sigma2^2 ))/sigma2 )/( 3*sqrt(2*pi) )
}

r.post <- function()
{
  if ( runif(1)< 1/3 ) return( rnorm(1,y,sigma1) )
  else                 return( rnorm(1,2*y,2*sigma2))
}

E.exact <- vector( length = 10000 )
for (i in 1:10000 ) E.exact[i] <- r.post()


sample <- NULL
for(i in 1:1000) sample <- c(sample,f.dist(1))
hist(sample,breaks=200)

entropy(sample)

r.prior <- function()
{
  runif(1)*3
}

d.prior <- function(theta) 1

y <- 1.
n.sample <- 10000
iter.max <- 60*n.sample

res <- SABC(r.model, r.prior, d.prior, 
                n.sample = n.sample, 
                eps.init = 0.2, 
                iter.max = iter.max, 
                v = 0.5, 
                beta=0.5,  
                delta=0.1, resample=5*n.sample, 
                verbose=n.sample, 
                method="noninformative", adaptjump=TRUE, 
                y = y)

plot(f.post, 0,2.5)
hist(res$E[,1], breaks=2000, freq=FALSE, add=TRUE)
length(which(res$E[,1]<1.5))
length(which(res$E[,1]>1.5))

hist(E.exact, breaks=2000, freq=FALSE, add=TRUE, col="red")
length(which(E.exact<1.5))
length(which(E.exact>1.5))

KL.divergence(res$E[,1], E.exact, k=100)

freqs1 <- discretize(res$E[,1], 1000, r=c(0,2.5))
freqs2 <- discretize(E.exact, 1000, r=c(0,2.5))
KL.plugin(freqs1,freqs2)

d1 <- density(E.exact)

