# ====================================================
#
# SABC with entropy measurements
#
# ====================================================

library(MASS)
library(FNN)
library(poweRlaw)

#library(entropy)

mass_ratio <- 10

sigma1 <- .1
sigma2 <- .5/mass_ratio
  
prior_range <- 3.5

y <- 0.2

#plot(f.post, 0,prior_range)

# model:

r.model <- function(theta)
{
  if (runif(1)<0.5) return(rnorm(1,theta, sigma1))
  else return(rnorm(1,theta/mass_ratio,sigma2))
}

# prior:

r.prior <- function()
{
  runif(1)*prior_range
}

d.prior <- function(theta) 1/prior_range

# posterior:

f.post <- function(theta)
{
  (exp(-( theta - y )^2/( 2*sigma1^2 ))/sigma1 +
     exp(-( theta/mass_ratio - y )^2/( 2*sigma2^2 ))/sigma2 )/( (mass_ratio+1)*sqrt(2*pi) )
}

integrate(f.post,-100,100)

r.post <- function()
{
  if ( runif(1)< 1/(mass_ratio+1) ) return( rnorm(1,y,sigma1) )
  else return( rnorm(1,mass_ratio*y,mass_ratio*sigma2))
}

# Sample from exact posterior:

E.exact <- vector( length = n.sample )
for (i in 1:n.sample ) E.exact[i] <- r.post()


# SABC with entropy measurements:
#================================

##------------------------
## Define a few functions:
##------------------------

## Define functions for moments of mu:
dim.par <- length(r.prior())

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
  log(
      exp( - (x-theta   )^2/(2*sigma1^2) ) / sigma1   + 
      exp( - (x-theta/mass_ratio)^2/(2*sigma2^2) ) / sigma2 
     ) 
}

# Set SABC parameters:
#--------------------

# best so far: iter.max = 50*n.sample, beta = 0.05, there seems to be a minimal
# entropy production for v between 2 and 100.

n.sample <- 10000
iter.max <- 50*n.sample
verbose  <- n.sample
eps.init <- 0.3
v <- 3
beta <- 0.4

s <- 0.0001

adaptjump = FALSE

# vs <- c(5,7,10,20,300)
# betas <- 0.4
# EP <- matrix(nrow=length(vs),ncol=length(betas))
# KL <- matrix(nrow=length(vs),ncol=length(betas))
# 
# for(v_counter in 1:length(vs)){  
# for(beta_counter in 1:length(betas) ){
# 
#   v <- vs[v_counter]
#   beta <- betas[beta_counter]

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

# offset

offset <- iter

eps_t <- vector(length=iter.max)
eps_t_counter <- 1

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
    eps_t[eps_t_counter] <- eps
    eps_t_counter <- eps_t_counter + 1
    
  }
  
  iter <- iter + verbose
  counter <- counter + 1
  
  ## Calculate system entropy:
  
  E.scaled <- E[,c(1,3)]
  E.scaled[,2] <- (E.scaled[,2]-y)/U
  ent.scaled <- entropy(E.scaled,k=10)[10]
  
  entropy.system[counter] <- ent.scaled + log(U)

  entropy.system[counter] <- entropy(E[,c(1,3)],k=10)[10]
  
  ## Calculate entropy production:
  
  entropy.difference <- entropy.system[counter-1] - entropy.system[counter]
  
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
# 
# plot(entropy.system)

# plot(E[,1],E[,3])
# points(P[,1],P[,3],col="green")


# Plot posterior:

plot(f.post, 0,prior_range)
hist(E[,1], breaks=n.sample/30, freq=FALSE, add=TRUE)
#hist(E.exact, breaks=n.sample/70, freq=FALSE, add=TRUE, col="red")
legend("topright", c(paste("iter.max =", iter.max),
                     paste("v =",v), 
                     paste("beta =",beta)))

# Check share of sample points in each hump:

# length(which(E[,1]<1.0))/length(which(E[,1]>1.0))
# length(which(E.exact<1.0))/length(which(E.exact>1.0))

# KL-divergence to target:

plot(KL.divergence(E.exact, E[,1], k=50))
legend("topright", c(paste("iter.max =", iter.max),
                     paste("v =",v), 
                     paste("beta =",beta)))

# ratio of rejections:

accept/iter.max

# plot decay of epsilon:

fn <- function(theta,y,N)
{
  sum(((y-theta[2]+theta[1]*log((theta[3]+1):(theta[3]+N))))^2)
}

plot(eps_t)
plot((8*offset+1):(8*offset+length(eps_t)),eps_t,log="xy")
eps_t <- eps_t[which(eps_t != 0)]

optim(par=c(2,24,9*offset),fn=fn,gr=NULL,log(eps_t),length(eps_t))

t <- (149281+1):(149281+length(eps_t))
plot(t,eps_t,log="xy")
lines(t,exp(24)*t^{-2.22},col="red")


# KL[v_counter,beta_counter] <- KL.divergence(E.exact, E[,1], k=50)[20] 
# 
# # total entropy production:
# 
# EP[v_counter,beta_counter] <- sum(entropy.production)
# 
# }}

# 
# image(KL, col=terrain.colors(30))
# 
# image(EP,col=terrain.colors(40))

# #check:
# 
# test1 <- rnorm(10000)
# test2 <- rnorm(10000,0,0.5)
# 
# hist(test1,freq=FALSE)
# hist(test2,add=TRUE,freq=FALSE,col="red")
# 
# plot(KL.divergence(test1,test2,k=30))
# points(-mean(log(dnorm(test1,0,0.5))) - entropy(test1,k=20),col="red")
# 
# plot(KL.divergence(test2,test1,k=30))
# points(-mean(log(dnorm(test2))) - entropy(test2,k=20),col="red")
# 
# 

