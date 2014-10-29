#===============================================================================
#
#  SABC.inf Algorithm
#
#===============================================================================

## ARGUMENTS:
## f.dist:    Function that either returns a random sample from the likelihood
##            or the distance between data and such a random sample
##            The first argument must be the parameter vector.
## d.prior:   Function that returns the density of the prior distribution
## r.prior:   Function which returns one vector of a random realization of the
##            prior distribution.
##
## n.sample:  desired number of samples
##
## eps.init:  initial epsilon
## iter.max   maximal number of iterations
## v:         tuning parameter
## beta:      tuning parameter
## delta:     tuning parameter
## resample:  after how many accepted updates?
##
## verbose:   shows progress each verbose outputs. Default is 1.
## adaptjump: whether to adapt covariance of jump distribution. Default is TRUE.
## y:         Data
## f.summarystats: function for automatic summary statistic
## ... :      further arguments passed to f.dist
##
## VALUES:
## A list with the following components:
## E:         matrix with ensemble of samples (row-wise)
## P:         matrix with prior ensemble of samples (row-wise)
## eps:       value of epsilon at final iteration
## ESS:       effective sample size
## -----------------------------------------------------------------------------

SABC.inf <- function(f.dist, d.prior, r.prior,
                     n.sample, eps.init, iter.max,
                     v, beta, delta,
                     resample=n.sample, verbose=1, adaptjump=TRUE,
                     summarystats=FALSE, y, f.summarystats=NULL, ...){
  ##-------------------------
  ## Define global variables:
  ##-------------------------

  ## Dimension of theta
  dim.par     <- length(r.prior())

  ## Has the prior sample been renewed?
  new.sample  <- FALSE

  ## Ensemble of particles
  E <- matrix(NA, nrow=n.sample, ncol=dim.par+2)

  ## Small constant for preventing degeneration of covariance matrix
  s <- 0.0001

  ## Positive constant as in eq. (51)
  a.force <- 6

  iter <- 0

  ##------------------------
  ## Define a few functions:
  ##------------------------

  ## Define mean U and V using P (42-43):
  X <- function(eps.1, eps.2){
    if(new.sample){
      SS <-  sum(exp(log.weights - P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]))
      U  <- (sum(exp(log.weights - P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]) * P[,dim.par +1 ])) / SS
      V  <- (sum(exp(log.weights - P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]) * P[,dim.par + 2])) / SS
    } else {
      SS <-  sum(exp(-P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]))
      U  <- (sum(exp(-P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]) * P[,dim.par + 1])) / SS
      V  <- (sum(exp(-P[,dim.par + 1] /
                 eps.1 - eps.2 * P[,dim.par + 2]) * P[,dim.par + 2])) / SS
    }
    return(c(U,V))
  }

  ## Define L using E and P (45):
  L <- function(eps.1, eps.2){
    U.sys <-  matrix(E[,dim.par+1], ncol=N.thin)
    V.sys <-  matrix(E[,dim.par+2], ncol=N.thin)
    chi <- ifelse(((U.sys - U.pri) / eps.1
                   + (1 + eps.2) * (V.sys - V.pri)) >= 0,1,0)

    if(new.sample)
      tmp <- k * exp(log.weights + V.pri) * chi
    else
      tmp <- k * exp(V.pri) * chi
    den <- n.sample * N.thin

    L.11 <- sum((U.sys - U.pri)^2 * tmp) / den
    L.22 <- sum((V.sys - V.pri)^2 * tmp) / den
    L.12 <- sum((U.sys - U.pri) * (V.sys - V.pri) * tmp) / den
    return(c(L.11,L.12,L.22))
  }

  ## Define schedule:

  Schedule <- function(eps.1, eps.2, eps.2.e){
    aa <- L(eps.1, eps.2)
    de <- eps.2 - eps.2.e
    disk <- aa[2]^2 * de^2 - aa[1] * (aa[3] * de^2 - v)
    kappa <- ifelse(disk > 0, (-aa[2] * de - sqrt(disk)) / aa[1], 0)
    return((1/eps.1 - 1/ kappa))
  }

  ## Define k density matrix:  TODO all

  K <- function(Covar.jump){
    jump.inv <- solve(Covar.jump)

    system.tensor <- rep(E[["alpha"=(1:dim.par),drop=FALSE]], times=N.thin,  pos=2, name="kp")
    Theta.tensor  <- add.tensor(system.tensor, prior.tensor, op="-")

    tmp <- mul.tensor(Theta.tensor,"alpha", Sigma.tensor, "alpha")
    k   <- mul.tensor(tmp,"beta", Theta.tensor, j="alpha",by=c("k","kp"))
    den <- (2*pi)^(dim.par/2)*sqrt(det(Covar.jump))
    return(exp(-0.5*k)/den)

  }

  # Effective sample size:

  ESS <- function(eps.1,eps.2){
    weights <- exp( -P[,dim.par + 1] / eps.1 - P[,dim.par + 2] * eps.2)
    weights <- weights/sum(weights)
    return(sum(weights^2)^(-1))
  }

  ## ------------------
  ## Initialization
  ## ------------------

  ## sample initial population

  if(summarystats)
  {
    # Sample from joint prior:

    dim.f <- length( f.summarystats( f.dist( r.prior(),... ) ) )
    PP    <- matrix(nrow=4*n.sample, ncol=dim.par + dim.f )       # Prior sample

    for( i in 1:(4*n.sample) )
    {
      theta.p <- r.prior()
      x.p   <- as.numeric( f.summarystats( f.dist(theta.p,... ) ) )
      PP[i,] <- c( theta.p,x.p )
    }

    iter <- 4*n.sample        # global counter for likelihood simulations

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

    for( i in 1:(4*n.sample) )
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

  } else {
    ## Create matrix of rejected and accepted particles,
    ## dynamic increasing of nrow
    P <- matrix(NA, nrow=2*n.sample, ncol=dim.par+2)

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
      rho.p <- f.dist(theta.p, ...)  # Calculate distance from target
      v.p     <- -log(d.prior(theta.p))  #Calculate likelihood


      ## Accept with Prob=exp(-rho.p/eps.init)
      if(runif(1) < exp(-rho.p / eps.init)){
        counter <- counter + 1
        E[counter,] <- c(theta.p, rho.p, v.p)
      }

      ## Add to P and increase its size if full
      if(iter > nrow(P))
          P <- rbind(P, matrix(NA, nrow=n.sample, ncol=dim.par + 2))
      P[iter,] <- c(theta.p, rho.p, v.p)
    }

    ## Remove empty rows of P due to dynamic allocation
    P <- P[1:iter,]
  }
  N.prior  <- nrow(P)

  ## Define initial epsilons:
  eps.1 <- eps.init
  eps.2 <- 0

  ## Jump covariance matrix of parameters

  Covar.jump <- beta * var(E[,1:dim.par]) + s * diag(1,dim.par)

  ## draw a smaller sample from prior and prepare tensors:

  N.thin <- 300

  indices.2 <- sample(1:N.prior, N.thin)

  U.pri <-  matrix(P[indices.2,dim.par+1]),ncol=n.sample)

  V.pri <- matrix(P[indices.2,dim.par+2], ncol=n.sample)

  prior.tensor  <- rep(to.tensor(as.vector(P[indices.2,1:dim.par]),dims=c(kp=N.thin,alpha=dim.par)),
                       times=n.sample,pos=1,name="k" )




  # Calculate k density matrix:

  k <- K(Covar.jump)

  # find initial epsilons:

  eps.2.e   <- 0
  eps.1.e   <- Schedule(eps.init,eps.2,eps.2.e)

  # Calculate mean U and V:

  U <- mean.tensor(E[["alpha"=dim.par+ 1]],along="k")
  V <- mean.tensor(E[["alpha"=dim.par+ 2]],along="k")

  # Determine critical epsilon:

  tmp      <- Vectorize(function(epsilon) abs(as.numeric(ESS(epsilon,0))-50))
  eps.crit <- optimize(tmp,c(0,0.5))$minimum


  ##--------------
  ## 2. iteration
  ##--------------

  a  <- 0    # acceptance counter
  kk <- 1    # update counter

  update.interval <- round(n.sample / 2)

  while (kk < (iter.max-iter) )
  {
    # Show progress:

    if(!is.null(verbose) && kk %% verbose == 0)
    {
      cat('ensemble updates' , (kk+iter)/n.sample, '\t',
          # 'eps.1.e '  , eps.1.e, '\t',
          # 'eps.2.e'   , eps.2.e, '\t',
          'eps.1'     , eps.1,   '\t',
          'eps.2'     , eps.2,   '\n'
          # 'U'  , U, '\t',
          #  'V'  , V, '\n'
      )
    }

    ## 2.1 select one arbitrary particle:

    index <- sample(1:n.sample,1)

    ## sample proposal parameter and calculate u and v:

    theta.p <- as.vector(E[index,1:dim.par] + to.tensor(mvrnorm(1, mu=rep(0, dim.par), Sigma=Covar.jump)))
    u.p     <- f.dist(theta.p,...)
    v.p     <- -log(d.prior(theta.p))

    ## 2.3 acceptance probability:

    Prob.accept  <- min(1,
                        exp(
                          (E[index,dim.par+1] - u.p)/eps.1.e+
                            (1+eps.2.e)*(E[index,dim.par+2]-v.p))
                        )

    ## 2.4 if accepted:

    if(runif(1) < Prob.accept)
    {
      # Update E:

      E[index,1:dim.par] <- theta.p
      E[index,dim.par+1] <- u.p
      E[index,dim.par+2] <- v.p


      if(a %% update.interval == 0)
      {

        ## update U and V:

        U.old <- U
        U <- mean.tensor(E[["alpha"=dim.par+ 1]],along="k")

        V.old <- V
        V <- mean.tensor(E[["alpha"=dim.par+ 2]],along="k")

        # Update (u,v)-covariance:

        Cov.uv <- cov(E[,(dim.par+1):(dim.par+2)])
        Cov.uv.inv <- solve(Cov.uv)


        ## update epsilons:

        repeat
        {
          eps.new <- c(1/eps.1,eps.2) - Cov.uv.inv %*% (c(U,V)-c(U.old,V.old))
          eps.1 <- 1/eps.new[1]
          eps.2 <- eps.new[2]
          X.soll <- X(eps.1,eps.2)
          error <- abs(c(U,V)-X.soll)/c(U,V)
          if (error[1] <= 0.01 & error[2] <= 0.01) break
          else
          {
            U.old <- X.soll[1]
            V.old <- X.soll[2]
          }
        }

        ## update covariance:

        if(adaptjump)
        {
          Covar <- var(E[,1:dim.par])

          # Update k:

          Covar.jump <- beta*Covar + s*diag(1,dim.par)
          k <- K(Covar.jump)
        }

        # Update equilibrium epsilons:

        eps.2.e <- max(-0.99,-eps.2*a.force)
        eps.1.e <- Schedule(eps.1,eps.2,eps.2.e)

        # Renew sample, if necessary:

        if( ESS(eps.1,eps.2) < 50 & new.sample == FALSE )
        {
          print("sample is renewed!!")

          new.sample <- TRUE
          Z <- sum(exp(-P[,dim.par+1]/eps.1 - eps.2*P[,dim.par+2]))/N.prior
          P <- E
          log.weights <- E[,dim.par+1]/eps.1 + eps.2*E[,dim.par+2] + log(Z)

          N.thin <- 300

          indices.2 <- sample(1:n.sample, N.thin)

          U.pri.ten <-  rep(to.tensor(P[indices.2,dim.par+1]),
                            times=n.sample,  pos=1, name="k")
          names(U.pri.ten) <- c("k","kp")

          V.pri.ten <-  rep(to.tensor(P[indices.2,dim.par+2]),
                            times=n.sample,  pos=1, name="k")
          names(V.pri.ten) <- c("k","kp")

          prior.tensor  <- rep(to.tensor(as.vector(P[indices.2,1:dim.par]),dims=c(kp=N.thin,alpha=dim.par)),
                               times=n.sample,pos=1,name="k" )

          log.weights.ten <- rep(log.weights[indices.2], times = n.sample, pos=1, name="k")
          names(log.weights.ten) <- c("k","kp")
        }

      }

      ## increment acceptance counter:

      a <- a+1
    }

    ## 2.5 resampling

    if(a == resample)
    {
      ## weighted resampling
      w         <- exp(-E[,dim.par+1]*delta/eps.1 + eps.2*E[,dim.par+2])
      w         <- w/sum(w)
      index     <- sample(1:n.sample, n.sample, replace=TRUE, prob=w)
      E         <- E[index,, drop=FALSE]

      # Update U, V and epsilons:

      eps.1     <- eps.1*(1-delta)
      eps.2     <- 0
      U <- mean.tensor(E[["alpha"=dim.par+ 1]],along="k")
      V <- mean.tensor(E[["alpha"=dim.par+ 2]],along="k")

      # Update equilibrium epsilons:

      eps.2.e <- max(-0.99,-eps.2*a.force)
      eps.1.e <- Schedule(eps.1,eps.2,eps.2.e)

      ## print effective sampling size
      cat("Resampling. Effective sampling size: ", 1/sum((w/sum(w))^2), "\n")

      ## update covariance
      Covar <- var(E[,1:dim.par])
      a <- 0
    }

    kk <- kk+1

  }

  # final bias correction:

  w         <- exp(-E[,dim.par+1]*delta/eps.1 + eps.2*E[,dim.par+2])
  w         <- w/sum(w)
  index     <- sample(1:n.sample, n.sample, replace=TRUE, prob=w)
  E         <- E[index,, drop=FALSE]

  #--------
  # Output
  #--------

  return(list(E=E, P=P, eps=eps.1, ESS=1/sum((w/sum(w))^2) ))

}
