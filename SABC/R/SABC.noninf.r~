#========================================================
#
#  SABC.noninf Algorithm
#
#========================================================

## ARGUMENTS:
## f.dist:    Function that either returns a random sample from the likelihood
##            or the distance between data and such a random sample 
##            The first argument must be the parameter vector.
## d.prior:   Function that returns the density of the prior distribution 
## r.prior:   Function which returns one vector of a random realization of the
##	          prior distribution.
##
## n.sample:  desired number of samples
##
## eps.init:  initial epsilon
## iter.max   maximal number of iterations
## v:         tuning parameter
## beta:      tuning parameter
##
## verbose:   shows the iteration progress each verbose outputs. NULL for no output.
## adaptjump: whether to adapt covariance of jump distribution. Default is TRUE.
## ... :      further arguments passed to f.dist
##
## VALUES:
## A list with the following components:
## E:         matrix with ensemble of samples
## P:         matrix with prior ensemble of samples
## eps:       value of epsilon at final iteration
## 
## -------------------------------------------------------------------

SABC.noninf <- function (f.dist, d.prior, r.prior, n.sample, eps.init, iter.max, 
                         v, beta, delta, resample, verbose, adaptjump=adaptjump, 
                         summarystats=summarystats, y=y, f.summarystats=f.summarystats, ...) 
{
  
  ## Initialize all containers:
  ##---------------------------
  
  dim.par <- length(r.prior())
  E       <- matrix(NA, nrow=n.sample, ncol=dim.par+1)
  E.in    <- NULL
  
  s <- 0
  
  ## Define a few functions:
  ##------------------------
  
  # Define functions for moments of mu:
  
  Rho.mean <- function(epsilon)
  {
    if(epsilon<0) stop("negative epsilon")
    SS     <-  sum(exp(-P[,dim.par+1]/epsilon))
    first  <- (sum(exp(-P[,dim.par+1]/epsilon)*P[,dim.par+1]))/SS
    return(first)
  }
  
  # Define schedule:
  
  Schedule <- function(rho)
  {
    tmp <- function(epsilon) epsilon^2+v*epsilon^(3/2)-rho^2
    return(uniroot(tmp,c(0,rho))$root)
  }

  # Redefinition of metric:
  
  Phi <- function(rho)
  {
    sum(rho.old<rho)/N0
  }
  
  f.dist.new <- function(theta,...)
  {
    rho <- f.dist(theta,...)
    return(Phi(rho))
  }
  
  
  ## ------------------
  ## 1. initialization
  ## ------------------
  
  ## 1.1 sample initial population
  
  if(summarystats)
  {
    # Sample from joint prior:
    
    dim.f <- length( f.summarystats( f.dist( r.prior(),... ) ) )
    PP    <- matrix(nrow=3*n.sample, ncol=dim.par + dim.f )       # Prior sample
    
    for( i in 1:(3*n.sample) ) 
    {
      theta.p <- r.prior()
      x.p   <- as.numeric( f.summarystats( f.dist(theta.p,... ) ) )
      PP[i,] <- c( theta.p,x.p ) 
    }
    
    iter <- 3*n.sample        # global counter for likelihood simulations
    
    # Linear regression:
    
    B  <- matrix( nrow = (dim.f+1), ncol = dim.par)           # Regression parameters
    XX <- cbind( PP[,(dim.par+1):ncol(PP)],rep(1,n.sample) )
    BB <- solve( t(XX) %*% XX ) %*% t(XX) 
    
    for (i in 1:dim.par)
    {
      B[,i] <- (BB %*% PP[,i])#[1:dim.f,]
    }
    
    # Calculate summary stats of data:
    
    y_ss <- t(B) %*% as.numeric( c(f.summarystats( y ),1) )
    
    # Redefine f.dist:
    
    f.dist.old <- f.dist
    
    f.dist <- function(par,...)
    {
      x_ss <- t(B) %*% as.numeric( c( f.summarystats( f.dist.old( par,... ) ),1 ) )
      return( sum( ( ( x_ss-y_ss )/y_ss )^2 ) )      
    }
    
    # Redefine prior sample P and initialize E:
    
    P <- NULL
    counter <- 1
    
    for( i in 1:(3*n.sample) ) 
    {
      theta.p <- PP[i,1:dim.par]
      v.p     <- -log(d.prior(theta.p))
      rho.p   <- f.dist(theta.p,... )
      P <- rbind(P,c(theta.p,rho.p,v.p))
      if( runif(1) < exp(-rho.p/eps.init) & counter <=n.sample )
      {
        E[counter,1:dim.par] <- t(theta.p)
        E[counter,dim.par+1] <- rho.p
        E[counter,dim.par+2] <- v.p
        counter <- counter + 1
      }
    }
    
    # Add more sample points to E if necessary:
    
    while( counter <= n.sample) 
    {
      if(iter>iter.max) stop("'iter.max' reached! No initial sample could be generated.")
      
      # Propose particle:
      
      theta.p <- r.prior()
      v.p     <- -log(d.prior(theta.p))
      rho.p   <- f.dist(theta.p,...)
      
      # Store particle in prior matrix:
      
      P <- rbind(P,c(theta.p,rho.p,v.p))
      
      # If close enough to target, store particle in ensemble matrix:
      
      if( runif(1) < exp(-rho.p/eps.init) )
      {
        E[counter,1:dim.par] <- t(theta.p)
        E[counter,dim.par+1] <- rho.p
        E[counter,dim.par+2] <- v.p
        counter <- counter + 1
      }
      iter <- iter + 1
    }
    
  }
  else
  {
    iter <- 1
    i <- 1
    
    while(i<=n.sample) 
    {
      theta.p <- r.prior()                # proposal parameter vector
      rho.p <- f.dist(theta.p,...)
      iter <- iter+1
      if(iter>iter.max) stop("'iter.max' reached! No initial sample could be generated.")
      ## accept with Prob=exp(-rho.p/eps.init)
      if( runif(1) < exp(-rho.p/eps.init) )
      {
        E[i,1:dim.par] <- t(theta.p)
        E[i,dim.par+1] <- rho.p
        i <- i+1
      }
      else  E.in <- rbind(E.in, c(t(theta.p),rho.p))
    }
    P <- rbind(E,E.in)  
  }
  
  N0      <- nrow(P)
  rho.old <- P[,dim.par+1]
  
  # Redefine distances:

  E[,dim.par+1] <- apply(as.matrix(E[,dim.par+1]),1,Phi)
  
  P[,dim.par+1] <- apply(as.matrix(P[,dim.par+1]),1,Phi)
  
  # find initial epsilon_1:
  
  U   <- Rho.mean(eps.init)
  eps <- Schedule(U)
  
  # Define jump distribution:
  
  Covar.jump <- beta* var(E[,1:dim.par]) + s*diag(1,dim.par)
  
  ##--------------
  ## 2. iteration
  ##--------------
  
  a  <- 0       # acceptance counter
  kk <- 1       # iteration  counter
  
  while (kk < (iter.max-iter) )
  {
    # Show progress:
    
    if(!is.null(verbose) && kk %% verbose == 0)  
    {
      cat('updates' , (kk+iter)/n.sample, '\t',
          'eps '  , eps, '\t', 
          'u.mean'  , U, '\n'
          )
      a <- 0
    }
    
    # Select one arbitrary particle:
    
    index <- sample(1:n.sample,1)
    
    # Sample proposal parameter and calculate new distance:
    
    theta.p <- E[index,1:dim.par] + mvrnorm(1, mu=rep(0, dim.par), Sigma=Covar.jump)
    rho.p   <- f.dist.new(theta.p,...)
        
    # Calculate acceptance probability:
    
    prior.prob  <- d.prior(theta.p) / d.prior(E[index,1:dim.par])
    likeli.prob <- exp((E[index,dim.par+1] - rho.p)/eps)
    Prob.accept <- min(1, prior.prob * likeli.prob)
    
    # If accepted:
    
    if(runif(1)<Prob.accept) 
    {
      # Update E:
      
      E[index,1:dim.par] <- theta.p
      E[index,dim.par+1] <- rho.p
      
      # Optionally: Update jump distribution:
      
      if(adaptjump) Covar.jump <- beta*var(E[,1:dim.par]) + s*diag(1,dim.par)
      
      # Update U:
      
      U  <- mean(E[,dim.par+1])
      
      # Update epsilon:
      
      eps <- Schedule(U)
      
      # increment acceptance counter:
      
      a <- a+1
    }
    
    ## 2.5 resampling
    
    if(a == resample)
    {
      # weighted resampling:
      
      w         <- exp(-E[,dim.par+1]*delta/eps)
      w         <- w/sum(w)
      index     <- sample(1:n.sample, n.sample, replace=TRUE, prob=w)
      E         <- E[index,, drop=FALSE]
      
      # Update U and epsilon:
      
      eps     <- eps*(1-delta)
      U       <- mean(E[,dim.par+1])
      
      # Update equilibrium epsilon:
      
      eps <- Schedule(U)
      
      ## print effective sampling size
      cat("Resampling. Effective sampling size: ", 1/sum((w/sum(w))^2), "\n")
      
      ## update covariance
      Covar <- var(E[,1:dim.par])
      a <- 0
    }
    
    kk <- kk+1

  }
  
  #--------
  # Output
  #--------
  
  return( list( E=E, P=P, eps=eps ) )  

}