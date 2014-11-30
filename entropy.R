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

# calculate exact entropy for sum of two normals:



# SABC with entropy measurements:
#================================

n.sample <- 10000
iter.max <- 50*n.sample
verbose  <- n.sample
eps.init <- 0.2
v <- 2.
beta <- 0.9

s <- 0.0001

adaptjump = FALSE

dim.par <- length(r.prior())

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

# Define log-likelihood:

loglikeli <- function(x,theta)
{
  log(1+
        (sigma1/sigma2)*
        exp(
          - (x-theta/2)^2/(2*sigma2^2) + 
            (x-theta)^2  /(2*sigma1^2)
           ) 
      ) -   (x-theta)^2/(2*sigma1^2)
}

## ------------------
## Initialization
## ------------------

E <- matrix(NA, nrow=n.sample, ncol=dim.par+2)
P <- matrix(NA, nrow=2*n.sample, ncol=dim.par+2)

iter <- 0
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
  x.p <- r.model(theta.p)
  rho.p <- (x.p - y)^2  # Calculate distance from target
  
  ## Accept with Prob=exp(-rho.p/eps.init)
  if(runif(1) < exp(-rho.p / eps.init)){
    counter <- counter + 1
    E[counter,] <- c(theta.p, rho.p, x.p)
  }
  
  ## Add to P and increase its size if full
  if(iter > nrow(P))
    P <- rbind(P, matrix(NA, nrow=n.sample, ncol=dim.par + 2))
  P[iter,] <- c(theta.p, rho.p, x.p)
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

entropy.system <- vector( length = ceiling((iter.max-iter)/verbose) )
entropy.production <- vector( length = ceiling((iter.max-iter)/verbose) )
entropy.production.endorev <- vector( length = ceiling((iter.max-iter)/verbose) )

# counter for system entropy calculations:
counter <- 1

E.scaled <- E[,c(1,3)]
E.scaled[,2] <- E.scaled[,2]/U
ent.scaled <- entropy(E.scaled,k=10)[10]
entropy.system[1] <- ent.scaled + log(U)
  
entropy.production[1] <- 0
entropy.production.endorev[1] <- 0

while (iter <= iter.max)
{
  entropy.flow <- 0
  entropy.prod <- 0
  for(i in 1:verbose)
  {
    ## Select one arbitrary particle:
    index <- sample(1:n.sample, 1)
    
    ## Sample proposal parameter and calculate new distance:
    theta.p <- E[index,1:dim.par] +
      mvrnorm(1, mu=rep(0, dim.par), Sigma=Covar.jump)
    x.p <- r.model(theta.p)
    rho.p   <- Phi((x.p - y)^2)
    if(is.na(rho.p))
      next()
    ## Calculate acceptance probability:
    prior.prob  <- d.prior(theta.p) / d.prior(E[index,1:dim.par])
    likeli.prob <- exp((E[index,dim.par+1] - rho.p) / eps)
    
    ## If accepted
    if(runif(1) < prior.prob * likeli.prob)
    {
      ## Increment entropy flow:
      
      loglikeli.old <- loglikeli(E[index,3],E[index,1])
      loglikeli.new <- loglikeli(x.p,theta.p)
      
      entropy.flow <- entropy.flow + 
        (E[index,2] - rho.p) / (n.sample*eps) + 
        (loglikeli.new - loglikeli.old)/n.sample
      
      ## Increment entropy production under endoreversibility assumption:
      
      entropy.prod <- entropy.prod + 
        (E[index,2] - rho.p) * ( 1/eps - 1/U ) / n.sample 
      
      ## Update E
      E[index,] <- c(theta.p, rho.p, x.p)
      
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
  counter <- counter + 1
  
  ## Calculate system entropy:
  
  E.scaled <- E[,c(1,3)]
  E.scaled[,2] <- E.scaled[,2]/U
  ent.scaled <- entropy(E.scaled,k=10)[10]
  
  entropy.system[counter] <- ent.scaled + log(U)
  
  ## Calculate entropy production:
  
  entropy.difference <- entropy.system[counter] - entropy.system[counter+1]
  
  entropy.production[counter] <- entropy.flow - entropy.difference 
  entropy.production.endorev[counter] <- entropy.prod
    
  
  ## Show progress:
  cat('updates' , iter / iter.max * 100, '% \t',
      'eps '  , eps, '\t',
      'u.mean'  , U, '\n')
}


# Plotting results:

plot(entropy.production)
points(entropy.production.endorev,col="red")

plot(entropy.system)
