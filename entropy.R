# ====================================================
#
# SABC with entropy measurements
#
# ====================================================

library(MASS)
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


# check entropy estimates based on FNN:
#======================================

# generate test data:

Sigma <- diag(c(1,0.00001))
test <- mvrnorm(10000, mu=c(0,0), Sigma=Sigma)
#plot(test)

# calculate exact entropy:

ent.exact <- (1+log(2*pi)) + 0.5*log(det(Sigma))

# calculate entropy based on nearest neighbors:

ent.est <- entropy(test,k=20, algorithm="kd_tree")

# scaling

eps <- 0.001
test.scaled <- test
test.scaled[,2] <- test.scaled[,2]/eps
ent.scaled <- entropy(test.scaled,k=20)
ent.est.2 <- ent.scaled + log(eps)

# entropy based on discretization:

#test.disc <- discretize2d(test.scaled[,1],test.scaled[,2],12,12)
#entropy(test.disc)

# plotting:

plot(ent.est,ylim=c(ent.exact*0.9,ent.exact*1.1))
points(ent.est.2, col="green")
abline(h=ent.exact,col="red")

error.rel <- abs(ent.est - ent.exact)/ent.exact
plot(error.rel)



# SABC with entropy measurements:
#================================
##---------------------------
## Initialize all containers:
##---------------------------

## Dimension of theta
dim.par <- length(r.prior())

## Ensemble, each row contains a particle (theta,rho)
E <- matrix(NA, nrow=n.sample, ncol=dim.par+1)

## Global counter which is bounded by iter.max
iter <- 0

## Small constant for preventing degeneration of covariance matrix
s <- 0.0001


##------------------------
## Define a few functions:
##------------------------

## Define functions for moments of mu:
Rho.mean <- function(epsilon)
  return( (sum(exp( -P[,dim.par + 1] / epsilon) * P[,dim.par + 1])) /
            sum(exp( -P[,dim.par + 1] / epsilon)))

## Define schedule:
Schedule <- function(rho)
  return( uniroot(function(epsilon) epsilon^2 + v * epsilon^(3 / 2) - rho^2,
                  c(0,rho))$root ) #  This v is const*v in (32)

## Redefinition of metric:
Phi <- function(rho)
  return(sum(rho.old < rho) / nrow(P))

f.dist.new <- function(theta, ...)
  return(Phi(f.dist(theta, ...)))


## ------------------
## Initialization
## ------------------

  ## Create matrix of rejected and accepted particles,
  ## dynamic increasing of nrow
  P <- matrix(NA, nrow=2*n.sample, ncol=dim.par+1)
  
  counter <- 0  # Number of accepted particles in E
  while(counter < n.sample){
    ## Show progress:
    if(!is.null(verbose) && iter %% verbose == 0)
      cat('Likelihoods calls ' , iter/iter.max*100, '% \n')
    
    ## Check if we reached maximum number of likelihood evaluations
    iter <- iter + 1
    if(iter > iter.max)
      stop("'iter.max' reached! No initial sample could be generated.")
    
    ## Generate new particle
    theta.p <- r.prior()  # Generate a proposal
    rho.p <- f.dist(theta.p)  # Calculate distance from target
    
    ## Accept with Prob=exp(-rho.p/eps.init)
    if(runif(1) < exp(-rho.p / eps.init)){
      counter <- counter + 1
      E[counter,] <- c(theta.p, rho.p)
    }
    
    ## Add to P and increase its size if full
    if(iter > nrow(P))
      P <- rbind(P, matrix(NA, nrow=n.sample, ncol=dim.par + 1))
    P[iter,] <- c(theta.p, rho.p)
  }
  
  ## Remove empty rows of P due to dynamic allocation
  P <- P[1:iter,]


## Distances in old metric
rho.old <- P[,dim.par + 1]

## Define distances in new metric
E[,dim.par+1] <- sapply(E[,dim.par+1], Phi, simplify=TRUE)
P[,dim.par+1] <- sapply(P[,dim.par+1], Phi, simplify=TRUE)

## Find initial epsilon_1:
U   <- Rho.mean(eps.init)
eps <- Schedule(U)

## Define jump distribution covariance
Covar.jump <- beta* var(E[,1:dim.par]) + s*diag(1,dim.par)

##--------------
## Iteration
##--------------

## Acceptance counter to determine the resampling step

accept  <- 0

## Entropy buckets:

entropy.system <- vector( length = (iter.max-iter)/verbose )
entropy.production <- vector( length = (iter.max-iter)/verbose )
entropy.production.endorev <- vector( length = (iter.max-iter)/verbose )


while (iter <= iter.max)
{
  
  for(i in 1:verbose){
    ## Select one arbitrary particle:
    index <- sample(1:n.sample, 1)
    
    ## Sample proposal parameter and calculate new distance:
    theta.p <- E[index,1:dim.par] +
      mvrnorm(1, mu=rep(0, dim.par), Sigma=Covar.jump)
    rho.p   <- f.dist.new(theta.p)
    if(is.na(rho.p))
      next()
    ## Calculate acceptance probability:
    prior.prob  <- d.prior(theta.p) / d.prior(E[index,1:dim.par])
    likeli.prob <- exp((E[index,dim.par+1] - rho.p) / eps)
    
    ## If accepted
    if(runif(1) < prior.prob * likeli.prob){
      ## Update E
      E[index,] <- c(theta.p, rho.p)
      
      ## Optionally: Update jump distribution covariance
      if(adaptjump)
        Covar.jump <- beta * var(E[,1:dim.par]) + s * diag(1,dim.par)
      
      ## Update U:
      U  <- mean(E[,dim.par + 1])
      
      ## Update epsilon:
      eps <- Schedule(U)
      
      ## Increment acceptance counter:
      accept <- accept + 1
    }
  }
  iter <- iter + verbose
  ## Show progress:
  cat('updates' , iter / iter.max * 100, '% \t',
      'eps '  , eps, '\t',
      'u.mean'  , U, '\n')
  
  ## Resampling
  if(accept >= resample){
    ## Weighted resampling:
    w <- exp(-E[,dim.par + 1] * delta / eps)
    w <- w/sum(w)
    index.resampled <- sample(1:n.sample, n.sample, replace=TRUE, prob=w)
    E <- E[index.resampled,]
    
    ## Update U and epsilon:
    eps <- eps * (1 - delta)
    U <- mean(E[,dim.par + 1])
    eps <- Schedule(U)
    
    ## Print effective sampling size TODO: sense of summing over these weights?
    cat("Resampling. Effective sampling size: ", 1/sum((w/sum(w))^2), "\n")
    accept <- 0
  }
}

