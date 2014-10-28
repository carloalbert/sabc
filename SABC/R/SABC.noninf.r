#===============================================================================
#
#  SABC.noninf Algorithm
#
#===============================================================================

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
## delta:     tuning parameter
## resample   after how many accepted updates?
## verbose:   shows progress each verbose outputs. Default is 1.
## adaptjump: whether to adapt covariance of jump distribution. Default is TRUE.
## summarystats: defaults to FALSE
## y:         Data
## f.summarystats: function for automatic summary statistic
## ... :      further arguments passed to f.dist, aside of the parameter.
##
## VALUES:
## A list with the following components:
## E:         matrix with ensemble of samples (row-wise)
## P:         matrix with prior ensemble of samples (row-wise)
## eps:       value of epsilon at final iteration
##
## -------------------------------------------------------------------

SABC.noninf <- function (f.dist, d.prior, r.prior,
                         n.sample, eps.init, iter.max,
                         v, beta, delta,
                         resample=n.sample, verbose=1, adaptjump=TRUE,
                         summarystats=FALSE, y, f.summarystats=NULL, ...){
  ##---------------------------
  ## Initialize all containers:
  ##---------------------------

  ## Dimension of theta
  dim.par <- length(r.prior())

  ## Each row contains a particle (theta,rho)
  E <- matrix(NA, nrow=n.sample, ncol=dim.par+1)

  ## Global counter which is bounded by iter.max
  iter <- 0

  ## Small constant for preventing degeneration of covariance matrix
  s <- 0


  ##------------------------
  ## Define a few functions:
  ##------------------------

  ## Define functions for moments of mu:
  Rho.mean <- function(epsilon)
      return( (sum(exp(-P[,dim.par+1]/epsilon)*P[,dim.par+1]))/
               sum(exp(-P[,dim.par+1]/epsilon)) )

  ## Define schedule:
  Schedule <- function(rho)
      return( uniroot(function(epsilon) epsilon^2+v*epsilon^(3/2)-rho^2,
                      c(0,rho))$root )

  ## Redefinition of metric:
  Phi <- function(rho)
    return( sum(rho.old<rho)/nrow(P))
  f.dist.new <- function(theta,...)
    return(Phi(f.dist(theta,...)))


  ## ------------------
  ## Initialization
  ## ------------------

  if(summarystats){
    ## Sample from joint prior:
    dim.f <- length( f.summarystats( f.dist( r.prior() ,...) ))
    P    <- matrix(nrow=3*n.sample, ncol=dim.par + dim.f )
    for( i in 1:(3*n.sample) ){
      theta.p <- r.prior()
      x.p   <- as.numeric( f.summarystats( f.dist(theta.p ,...) ))
      P[i,] <- c( theta.p,x.p )
    }
    iter <- 3*n.sample

    ## Do a linear regression for summary statistics:
    B  <- matrix( nrow = (dim.f+1), ncol = dim.par)   # Regression parameters
    for (i in 1:dim.par){
      fitcoef <- lm(P[,i]~P[,(dim.par+1):ncol(P)])$coefficients
      B[,i] <-c(fitcoef[2:(length(fitcoef))],fitcoef[1])  # Last is intercept
    }

    ## Calculate summary stats of data:
    y_ss <- t(B) %*% as.numeric( c(f.summarystats( y ),1) )

    ## Redefine f.dist:
    f.dist.old <- f.dist
    f.dist <- function(par,...){
      x_ss <- t(B) %*%
              as.numeric( c( f.summarystats( f.dist.old( par,... ) ),1 ) )
      return( sum( ( ( x_ss-y_ss )/y_ss )^2 ) )
    }

    ## Redefine prior sample P and initialize E:
    counter <- 0  # Number of particles in E
    for( i in 1:(3*n.sample) ){
      theta.p <- P[i,1:dim.par]
      rho.p   <- f.dist(theta.p,... )
      P[i,] <- c(theta.p,rho.p,rep(0,dim.f-1))  # Only rho counts
      if( runif(1) < exp(-rho.p/eps.init) && counter < n.sample ){
        counter <- counter + 1
        E[counter,] <- c(theta.p,rho.p)

      }
    }
    P <- P[,1:(dim.par+1)] # Drop 0s rows because distance has one dimension

    ## Add more sample points to E if necessary:
    while(counter<n.sample){
      ## Check if we reached maximum number of likelihood evaluations
      iter <- iter+1
      if(iter>iter.max)
          stop("'iter.max' reached! No initial sample could be generated.")

      ## Generate new particle
      theta.p <- r.prior() # Generate a proposal
      rho.p <- f.dist(theta.p,...) # Calculate distance from target

      ## Accept with Prob=exp(-rho.p/eps.init)
      if( runif(1) < exp(-rho.p/eps.init) ){
        counter <- counter+1
        E[counter,1:dim.par] <- t(theta.p)
        E[counter,dim.par+1] <- rho.p

      }

      ## Add to P and increase its size if full
      if(iter>nrow(P))
          P <- rbind(P,matrix(NA,nrow=n.sample,ncol=dim.par+1))
      P[iter,1:dim.par] <- t(theta.p)
      P[iter,dim.par+1] <- rho.p
    }
    ## Remove empty rows of P due to dynamic allocation
    P <- P[1:iter,]

  } else{  # No summarystats...
    ## Create matrix of rejected and accepted particles,
    ## dynamic increasing of nrow
    P <- matrix(NA, nrow=2*n.sample, ncol=dim.par+1)

    counter <- 0  # Number of accepted particles in E
    while(counter<n.sample){
      ## Check if we reached maximum number of likelihood evaluations
      iter <- iter+1
      if(iter>iter.max){
          print(E)
          stop("'iter.max' reached! No initial sample could be generated.")
      }

      ## Generate new particle
      theta.p <- r.prior()  # Generate a proposal
      rho.p <- f.dist(theta.p,...)  # Calculate distance from target

      ## Accept with Prob=exp(-rho.p/eps.init)
      if( runif(1) < exp(-rho.p/eps.init) ){
        counter <- counter+1
        E[counter,1:dim.par] <- t(theta.p)
        E[counter,dim.par+1] <- rho.p
      }

      ## Add to P and increase its size if full
      if(iter>nrow(P))
          P <- rbind(P,matrix(NA,nrow=n.sample,ncol=dim.par+1))
      P[iter,1:dim.par] <- t(theta.p)
      P[iter,dim.par+1] <- rho.p
    }

    ## Remove empty rows of P due to dynamic allocation
    P <- P[1:iter,]
  }

  ## Distances in old metric
  rho.old <- P[,dim.par+1]

  ## Define distances in new metric
  E[,dim.par+1] <- apply(as.matrix(E[,dim.par+1]),1,Phi)
  P[,dim.par+1] <- apply(as.matrix(P[,dim.par+1]),1,Phi)

  ## Find initial epsilon_1:
  U   <- Rho.mean(eps.init)
  eps <- Schedule(U)

  ## Define jump distribution covariance
  Covar.jump <- beta* var(E[,1:dim.par]) + s*diag(1,dim.par)

  ##--------------
  ## Iteration
  ##--------------

  ## Acceptance counter
  accept  <- 0

  while (iter <= iter.max){
    ## Show progress:
    if(!is.null(verbose) && iter %% verbose == 0)
      cat('updates' , iter/iter.max*100, '% \t',
          'eps '  , eps, '\t',
          'u.mean'  , U, '\n')


    ## Select one arbitrary particle:
    index <- sample(1:n.sample,1)
    ## Sample proposal parameter and calculate new distance:
    theta.p <- E[index,1:dim.par] +
               mvrnorm(1, mu=rep(0, dim.par), Sigma=Covar.jump)
    rho.p   <- f.dist.new(theta.p,...)

    ## Calculate acceptance probability:
    prior.prob  <- d.prior(theta.p) / d.prior(E[index,1:dim.par])
    likeli.prob <- exp((E[index,dim.par+1] - rho.p)/eps)
  #  Prob.accept <- min(1, prior.prob * likeli.prob)

    ## If accepted
    if(runif(1)< prior.prob*likeli.prob){
      ## Update E
      E[index,1:dim.par] <- theta.p
      E[index,dim.par+1] <- rho.p

      ## Optionally: Update jump distribution covariance
      if(adaptjump)
          Covar.jump <- beta*var(E[,1:dim.par]) + s*diag(1,dim.par)

      ## Update U:
      U  <- mean(E[,dim.par+1])

      ## Update epsilon:
      eps <- Schedule(U)

      # Increment acceptance counter:
      accept <- accept+1
    }

    ## Resampling
    if(accept == resample){
      ## Weighted resampling:
      w <- exp(-E[,dim.par+1]*delta/eps)
      w <- w/sum(w)
      index.resampled <- sample(1:n.sample, n.sample, replace=TRUE, prob=w)
      E <- E[index.resampled,, drop=FALSE]

      ## Update U and epsilon:
      eps <- eps*(1-delta)
      U <- mean(E[,dim.par+1])
      eps <- Schedule(U)

      ## Print effective sampling size
      cat("Resampling. Effective sampling size: ", 1/sum((w/sum(w))^2), "\n")
      accept <- 0
    }
    iter <- iter+1
  }

  #--------
  # Output
  #--------

  return( list( E=E, P=P, eps=eps ) )

}
