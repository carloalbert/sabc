# ====================================================
#
# SABC with entropy measurements
#
# ====================================================

library(FNN)
#library(entropy)

sigma1 <- .1
sigma2 <- .01

# model:

r.model <- function(theta)
{
  if (runif(1)<0.5) return(rnorm(1,theta, sigma1))
  else return(rnorm(1,theta/2,sigma2))
}

# prior:

r.prior <- function()
{
  runif(1)*3
}

d.prior <- function(theta) 1

# data

y <- 1.

# posterior:

f.post <- function(theta)
{
  (exp(-( theta - y )^2/( 2*sigma1^2 ))/sigma1 +
     exp(-( theta/2 - y )^2/( 2*sigma2^2 ))/sigma2 )/( 3*sqrt(2*pi) )
}

r.post <- function()
{
  if ( runif(1)< 1/3 ) return( rnorm(1,y,sigma1) )
  else return( rnorm(1,2*y,2*sigma2))
}

# SABC:

n.sample <- 1000
iter.max <- 50*n.sample

res <- SABC(r.model, r.prior, d.prior,
            n.sample = n.sample,
            eps.init = 0.2,
            iter.max = iter.max,
            v = 2,
            beta=0.2,
            delta=0.1, resample=5*n.sample,
            verbose=n.sample,
            method="noninformative", adaptjump=TRUE,
            y = y)

# Sample from exact posterior:

E.exact <- vector( length = n.sample )
for (i in 1:n.sample ) E.exact[i] <- r.post()

# Plot posterior:

plot(f.post, 0,2.5)
hist(res$E[,1], breaks=n.sample/20, freq=FALSE, add=TRUE)
hist(E.exact, breaks=n.sample/20, freq=FALSE, add=TRUE, col="red")

# Check share of sample points in each hump:

length(which(res$E[,1]<1.5))/length(which(res$E[,1]>1.5))
length(which(E.exact<1.5))/length(which(E.exact>1.5))

# Calculate KL divergence:
#=========================

# With nearest neighbours:

plot(KL.divergence(res$E[,1], E.exact, k=500))

# with density estimation:

dens        <- density(res$E[,1])
dens.exact  <- density(E.exact,from=min(dens$x), to=max(dens$x))
plot(dens)
points(dens.exact,col="red")

dx <- dens$x[2] - dens$x[1]
tmp <- dens$y*log(dens$y/dens.exact$y)
tmp[which(tmp==Inf)]=NA
KL <- sum(tmp,na.rm=TRUE)*dx
KL

sum(dens.exact$y)*dx
