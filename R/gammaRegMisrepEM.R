gammaRegMisrepEM <- function(formula, v_star, data, lambda = c(0.6,0.4), epsilon = 1e-08, maxit = 10000, maxrestarts = 20, verb = FALSE) {

  # Check to make sure the v_star
  # variable is in the dataframe object;
  if(!any(v_star == colnames(data))){
    stop(paste("variable", v_star, "not present in dataframe" ))
  }

  # The name of the misrepresented variable;
  v_star_name <- v_star

  # v_star object needs to be a vector of 1's and 0's,
  # with class 'numeric'
  # Note that the v_star object changes from being a character to a vector
  v_star <- data[, v_star_name]

  # If v_star is a numeric, then do nothing
  if(is.numeric(v_star)){

  }else{
    # But if it isn't numeric, then check to see if it's class is factor;
    if(is.factor(v_star)){
      # This is a safe way of coercing a factor to a numeric, while
      # retaining the original numeric vales
      v_star <- as.numeric(levels(v_star))[v_star]
    }else{
      # and if it's not numeric, and not a factor, then something is
      # seriously wrong;
      stop("v_star variable must be of class 'factor' or 'numeric'")
    }
  }

  # The v_star variable needs to be binary (has 2 unique values)
  if(length(unique(v_star)) != 2){
    stop("v_star variable must contain two unique values")
  }

  # Furthermore, the two unique values must be 0/1;
  if( sort(unique(v_star))[1] != 0 | sort(unique(v_star))[2] != 1 ){
    stop("v_star variable must be coded with ones and zeroes")
  }

  # Check to see if user supplied lambda vector is valid;
  if(sum(lambda) != 1){
    stop("Lambda vector must sum to one")
  }
  if(length(lambda) != 2){
    stop("Lambda vector must contain two elements")
  }

  # Check to see if the design matrix is degenerate;
  if( !is.null(alias(lm(formula = formula, data = data))$Complete) ){
    stop("Linear dependencies exist in the covariates")
  }

  # obtain initial values
  naive <- glm(formula = formula, family = "Gamma"(link = 'log'), data = data, x = TRUE, y = TRUE)

  # This is a final error check that is done to ensure that the v* variable is
  # also included in the formula specification;
  if( any(colnames(naive$x) == v_star_name) ){
  }else{
    stop("v_star variable must be specified in 'formula'")
  }

  coef.reg <- naive$coefficients
  alpha <- as.numeric(gamma.shape(naive))[1]
  coef.reg <- c("alpha" = alpha, coef.reg)
  theta <- coef.reg

  # Note that the first element in the coef.reg and theta vectors is
  # the gamma shape parameter.

  # Make design matrix
  # Note that the first column is a 1's column, for the intercept.
  x <- model.matrix(object = terms(formula), data = data)


  # This other design matrix is made by first setting the v* column within the dataframe
  # to be fixed at one.
  data[,v_star_name] <- 1

  # Notice capital X
  X <- model.matrix(object = terms(formula), data = data)


  # xbeta object is defined to be the linear combination of covariates that
  # are exclusive to X--does NOT include interactions with v* or v* itself.
  # Note that it also includes the intercept term as well.
  # In the event that there's only two covariates (v* and some other one) and
  # the intercept is omitted, we then need to avoid using '%*%' and instead use '*'
  if( length(theta[-1][ -grep(v_star_name, names(theta[-1])) ]) == 1 ){
    xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] * theta[-1][ -grep(v_star_name, names(theta[-1])) ] )
  }else{
    xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] %*% theta[-1][ -grep(v_star_name, names(theta[-1])) ] )
  }

  iter <- 0
  diff <- epsilon + 1
  attempts <- 1

  # The response
  y <- naive$y

  # Used later for computing p-values
  n <- length(y)

  # observed loglikelihood
  obs.ll <- function(lambda, coef){
    sum(   v_star * log(          dgamma(x = y, shape = coef[1], rate = coef[1]/exp( x %*% coef[-1] )) )) +
      sum((1-v_star)* log(lambda[2]*dgamma(x = y, shape = coef[1], rate = coef[1]/exp( X %*% coef[-1] ))    +
                            lambda[1]*dgamma(x = y, shape = coef[1], rate = coef[1]/exp( x %*% coef[-1] ))))
  }

  # M step loglikelihood
  mstep.ll <- function(theta, z){
    -sum(                  log(dgamma(x = y[v_star==1], shape = theta[1], rate = theta[1]/exp( x %*% theta[-1] )[v_star == 1] ))) -
      sum((1- z[v_star==0])*log(dgamma(x = y[v_star==0], shape = theta[1], rate = theta[1]/exp( X %*% theta[-1] )[v_star == 0]))  +
            z[v_star==0] *log(dgamma(x = y[v_star==0], shape = theta[1], rate = theta[1]/exp( x %*% theta[-1] )[v_star == 0])))
  }

  old.obs.ll <- obs.ll(lambda, coef.reg)
  ll <- old.obs.ll


  # Number of digits (to the right of decimal point) printed to console will
  # depend on default user settings;
  num_digits <- getOption("digits")

  while(diff > epsilon && iter < maxit){

    # E-step
    dens1 <- lambda[1]*dgamma(x = y, shape = theta[1], rate = theta[1]/exp(xbeta))
    dens2 <- lambda[2]*dgamma(x = y, shape = theta[1], rate = theta[1]/exp( X %*% theta[-1] ))
    z <- dens1/(dens1 + dens2)
    lambda.hat <- c(mean(z[v_star == 0]), (1-mean(z[v_star == 0])))

    #Non-linear Minimization
    m <- try(suppressWarnings(nlm(f = mstep.ll, p = theta, z = z)), silent = TRUE)
    theta.hat <- m$estimate

    # Annoyingly, nlm() does not provide m$estimate as a named vector,
    # which consequently makes updating the xbeta object impossible.
    names(theta.hat) <- names(theta)

    new.obs.ll <- obs.ll(lambda.hat, theta.hat)
    diff <- new.obs.ll - old.obs.ll
    old.obs.ll <- new.obs.ll
    ll <- c(ll, old.obs.ll)
    lambda <- lambda.hat
    theta <- theta.hat


    if( length(theta[-1][ -grep(v_star_name, names(theta[-1])) ]) == 1 ){
      xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] * theta[-1][ -grep(v_star_name, names(theta[-1])) ] )
    }else{
      xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] %*% theta[-1][ -grep(v_star_name, names(theta[-1])) ] )
    }

    iter <- iter + 1


    # If TRUE, print EM routine updates to the console;
    if(verb){

      message("iteration = ", iter,
              " log-lik diff = ", format(diff, nsmall = num_digits),
              " log-like = ", format(new.obs.ll, nsmall = num_digits) )


    }

    # stop execution and throw an error if the max iterations has been reached,
    # and if the max num. of attempts has been made;
    if(iter == maxit && attempts == maxrestarts){
      stop("NOT CONVERGENT! Failed to converge after ", attempts,  " attempts", call. = F)
    }

    # If the max iterations is reached, but we can make another attempt, then
    # restart the EM routine with new mixing prop., but only notify user
    # of this if verb = TRUE
    if(iter == maxit && attempts < maxrestarts){

      if(verb){
        warning("Failed to converge. Restarting with new mixing proportions", immediate. = TRUE,
                call. = FALSE)
      }

      # Update the number of attempts made.
      attempts <- attempts + 1

      # Reset iter to zero
      iter <- 0

      cond <- TRUE

      while(cond){
        lambda.new <- c(0,0)
        lambda.new[2] <- runif(1)
        lambda.new[1] <- 1-lambda.new[2]
        if(min(lambda.new) < 0.15){
          cond <- TRUE
          lambda <- lambda.new
        }else{
          cond <- FALSE
        }
      }

      # With the new mixing proportions, re-calculate the old.obs.ll,
      old.obs.ll <- obs.ll(lambda, coef.reg)
      ll <- old.obs.ll

    }

  }

  # After the EM routine finishes, print how many iterations were performed.
  message("number of iterations = ", iter)

  # Make empty Hessian matrix;
  hess <- matrix(data = 0,  nrow = length(theta) + 1, ncol = length(theta) + 1,
                 dimnames = list( c("lambda", names(theta)), c("lambda", names(theta)) )  )

  shape <- as.numeric(theta[1])

  # Element (1,1)
  hess[1,1] <- -sum( (1 - v_star) * ( ( ( exp( -shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) - exp( -shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) ) ) / ( ( lambda[2]*exp( -shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) + lambda[1]*exp( -shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) ) ) )^2 )

  # Element (2,2)
  hess[2,2] <- sum( v_star * (1/shape - trigamma(shape)) + (1-v_star) * ( 1/shape - trigamma(shape) + ( lambda[2]*lambda[1]*exp(-shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1]))  ) * ( (X-x)%*%theta[-1] + y*(1/exp(X%*%theta[-1]) - 1/exp(x%*%theta[-1])) )^2 ) / ( lambda[2]*exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]))) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]))) )^2 ) )

  # Main diagonal elements that pertain to regression coefficients
  for(j in 1:ncol(x)){
    k <- j

    hess[j+2,k+2] <- -shape * sum( v_star*y*x[,j]*x[,k]/exp(x%*%theta[-1]) + (1-v_star)* ( lambda[2]^2*exp( -(X%*%theta[-1])*(2*shape+1) - 2*y*shape/exp(X%*%theta[-1]) ) * y*X[,j]*X[,k] - lambda[2]*lambda[1]*exp( -shape*(X + x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( shape*( x[,k]*(y/exp(x%*%theta[-1]) - 1) - X[,k]*(y/exp(X%*%theta[-1]) - 1)  ) * ( x[,j]*(y/exp(x%*%theta[-1]) - 1) - X[,j]*(y/exp(X%*%theta[-1]) - 1) ) - y*x[,j]*x[,k] /exp(x%*%theta[-1]) - y*X[,j]*X[,k] / exp(X%*%theta[-1]) ) + lambda[1]^2*exp( -(x%*%theta[-1])*(2*shape+1) - 2*y*shape /exp(x%*%theta[-1]) )*y*x[,j]*x[,k] ) / ( lambda[2]*exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) )^2 )

  }

  # Off-diagonal elements that pertain to regression coefficients;
  for(i in 1:choose(ncol(x), 2)){

    j <- combn(x = 1:ncol(x), m = 2)[1,i] # Column index
    k <- combn(x = 1:ncol(x), m = 2)[2,i] # Row index

    hess[k + 2, j + 2] <- -shape * sum( v_star*y*x[,j]*x[,k]/exp(x%*%theta[-1]) + (1-v_star)* ( lambda[2]^2*exp( -(X%*%theta[-1])*(2*shape+1) - 2*y*shape/exp(X%*%theta[-1]) ) * y*X[,j]*X[,k] - lambda[2]*lambda[1]*exp( -shape*(X + x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( shape*( x[,k]*(y/exp(x%*%theta[-1]) - 1) - X[,k]*(y/exp(X%*%theta[-1]) - 1)  ) * ( x[,j]*(y/exp(x%*%theta[-1]) - 1) - X[,j]*(y/exp(X%*%theta[-1]) - 1) ) - y*x[,j]*x[,k] /exp(x%*%theta[-1]) - y*X[,j]*X[,k] / exp(X%*%theta[-1]) ) + lambda[1]^2*exp( -(x%*%theta[-1])*(2*shape+1) - 2*y*shape /exp(x%*%theta[-1]) )*y*x[,j]*x[,k] ) / ( lambda[2]*exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) )^2 )

    hess[j + 2, k + 2] <- -shape * sum( v_star*y*x[,j]*x[,k]/exp(x%*%theta[-1]) + (1-v_star)* ( lambda[2]^2*exp( -(X%*%theta[-1])*(2*shape+1) - 2*y*shape/exp(X%*%theta[-1]) ) * y*X[,j]*X[,k] - lambda[2]*lambda[1]*exp( -shape*(X + x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( shape*( x[,k]*(y/exp(x%*%theta[-1]) - 1) - X[,k]*(y/exp(X%*%theta[-1]) - 1)  ) * ( x[,j]*(y/exp(x%*%theta[-1]) - 1) - X[,j]*(y/exp(X%*%theta[-1]) - 1) ) - y*x[,j]*x[,k] /exp(x%*%theta[-1]) - y*X[,j]*X[,k] / exp(X%*%theta[-1]) ) + lambda[1]^2*exp( -(x%*%theta[-1])*(2*shape+1) - 2*y*shape /exp(x%*%theta[-1]) )*y*x[,j]*x[,k] ) / ( lambda[2]*exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) )^2 )

  }


  # Element (2,1), (1,2)
  hess[2,1] <- sum( (1-v_star) * ( exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( (x - X)%*%theta[-1] + y*(1/exp(x%*%theta[-1]) - 1/exp(X%*%theta[-1])) ) ) / ( lambda[2]*exp( -shape*(X%*%theta[-1] + y/exp(X%*%theta[-1])) ) + lambda[1]*exp( -shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) )^2 )
  hess[1,2] <- sum( (1-v_star) * ( exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( (x - X)%*%theta[-1] + y*(1/exp(x%*%theta[-1]) - 1/exp(X%*%theta[-1])) ) ) / ( lambda[2]*exp( -shape*(X%*%theta[-1] + y/exp(X%*%theta[-1])) ) + lambda[1]*exp( -shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) )^2 )


  # Covariances of regression coefs -- lambda
  for(j in 1:ncol(x)){
    hess[j+2, 1] <- shape * sum( (1-v_star) * ( exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( X[,j]*(y/exp(X%*%theta[-1]) - 1) - x[,j]*(y/exp(x%*%theta[-1]) - 1) ) ) / ( lambda[2] * exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]))) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) )^2 )

    hess[1, j+2] <- shape * sum( (1-v_star) * ( exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( X[,j]*(y/exp(X%*%theta[-1]) - 1) - x[,j]*(y/exp(x%*%theta[-1]) - 1) ) ) / ( lambda[2] * exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]))) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) )^2 )

  }

  # Covariances of regression coefs -- alpha (dispersion param)
  for(k in 1:ncol(x)){
    hess[k+2, 2] <- sum(v_star * x[,k]*(y/exp(x %*% theta[-1]) - 1) + (1-v_star)* ( lambda[2]^2 * exp( -2*shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) * X[,k]*(y/exp(X%*%theta[-1]) - 1) + lambda[2]*lambda[1]*exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( shape*( x[,k]*(y/exp(x%*%theta[-1]) - 1) - X[,k]*(y/exp(X%*%theta[-1]) - 1) ) * ( (X-x)%*%theta[-1] + y*(1/exp(X%*%theta[-1]) - 1/exp(x%*%theta[-1])) ) + x[,k]*(y/exp(x%*%theta[-1]) - 1) + X[,k]*(y/exp(X%*%theta[-1]) - 1) ) + lambda[1]^2 * exp( -2*shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) * x[,k]*(y/exp(x%*%theta[-1]) - 1) ) / ( lambda[2] * exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1])) ) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) )^2 )

    hess[2, k+2] <- sum(v_star * x[,k]*(y/exp(x %*% theta[-1]) - 1) + (1-v_star)* ( lambda[2]^2 * exp( -2*shape*(X%*%theta[-1] + y/exp(X%*%theta[-1]) ) ) * X[,k]*(y/exp(X%*%theta[-1]) - 1) + lambda[2]*lambda[1]*exp( -shape*(X+x)%*%theta[-1] - y*shape*(1/exp(X%*%theta[-1]) + 1/exp(x%*%theta[-1])) ) * ( shape*( x[,k]*(y/exp(x%*%theta[-1]) - 1) - X[,k]*(y/exp(X%*%theta[-1]) - 1) ) * ( (X-x)%*%theta[-1] + y*(1/exp(X%*%theta[-1]) - 1/exp(x%*%theta[-1])) ) + x[,k]*(y/exp(x%*%theta[-1]) - 1) + X[,k]*(y/exp(X%*%theta[-1]) - 1) ) + lambda[1]^2 * exp( -2*shape*(x%*%theta[-1] + y/exp(x%*%theta[-1])) ) * x[,k]*(y/exp(x%*%theta[-1]) - 1) ) / ( lambda[2] * exp(-shape*(X%*%theta[-1] + y/exp(X%*%theta[-1])) ) + lambda[1]*exp(-shape*(x%*%theta[-1] + y/exp(x%*%theta[-1]) ) ) )^2 )

  }

  # FIM is the negative of the  Hessian;
  FIM <- -hess

  # Then get std.errors;
  cov.pars.estimates <- solve(FIM)
  std.error <- sqrt(diag(cov.pars.estimates))

  # Calculate t values for regression coefficients
  t_vals <- rep(NA, length(theta[-1]))
  t_vals <- theta[-1] / std.error[-c(1,2)]


  # Calculate p-values of regression coefficients
  # argument df: '-1' because theta doesn't include lambda parameter.
  p_vals <- rep(NA, length(t_vals))
  p_vals <- 2 * pt(q = abs(t_vals), lower.tail = F, df = n - length(theta) - 1 )

  # AIC, AICc, BIC
  # Note that theta does not contain lamdba, hence the '+1' included.
  perf_metrics <- rep(NA, 3)
  AIC <- 2 * (length(theta) + 1 -  new.obs.ll)
  AICc <- AIC + (2 * (length(theta) + 1)^2 + 2 * (length(theta) + 1) )/(n - (length(theta) + 1) - 1)
  BIC <- log(n) * (length(theta) + 1) - 2 * new.obs.ll
  perf_metrics <- c(AIC, AICc, BIC)
  names(perf_metrics) <- c("AIC", "AICc", "BIC")


  # Output
  a <- list(y = y, lambda = lambda[2], params = theta, loglik = new.obs.ll,
            posterior = as.numeric(z), all.loglik = ll, cov.estimates = cov.pars.estimates,
            std.error = std.error, t.values = t_vals, p.values = p_vals,
            ICs = perf_metrics, ft = "gammaRegMisrepEM", formula = formula,
            v_star_name = v_star_name)
  class(a) <- "misrepEM"
  a

}

