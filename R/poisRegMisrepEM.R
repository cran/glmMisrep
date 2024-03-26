poisRegMisrepEM <- function(formula, v_star, data, lambda = c(0.6,0.4), epsilon = 1e-08, maxit = 10000, maxrestarts = 20, verb = FALSE) {


  # Simple checks to make sure the response and the v_star
  # variable are contained within the data object;
  if(!any(v_star == colnames(data))){
    stop(paste("variable", v_star, "not present in dataframe" ))
  }

  # The name of the misrepresented variable;
  v_star_name <- v_star

  # v_star object needs to be a vector of 1's and 0's,
  # with class 'numeric'
  # Note that v_star changes from being a character to a vector
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
  if(sort(unique(v_star))[1] != 0 | sort(unique(v_star))[2] != 1){
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
  naive <- glm(formula = formula, data = data, family="poisson"(link='log'), x = TRUE, y = TRUE)

  # This is a final error check that is done to ensure that the v* variable is
  # also included in the formula specification;
  if( any(colnames(naive$x) == v_star_name) ){
  }else{
    stop("v_star variable must be specified in 'formula'")
  }

  coef.reg <-naive$coefficients
  theta <-  coef.reg

  x <- model.matrix(object = terms(formula), data = data)

  # This other design matrix is made by first setting the v* column within the dataframe
  # to be fixed at one.
  data[,v_star_name] <- 1

  # Notice capital X
  X <- model.matrix(object = terms(formula), data = data)

  if( length(theta[ -grep(v_star_name, names(theta)) ]) == 1 ){
    xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] * theta[ -grep(v_star_name, names(theta)) ] )
  }else{
    xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] %*% theta[ -grep(v_star_name, names(theta)) ] )
  }

  iter <- 0
  diff <- epsilon+1
  attempts <- 1

  # The response
  y <- naive$y
  n <- length(y)

  # observed loglikelihood (partial LL, eq. 3 from Akakpo, Xia, Polansky 2018).
  obs.ll <- function(lambda,coef){
    sum(v_star*dpois(x = y, lambda = exp(x %*% coef), log = TRUE))+
      as.numeric(mylogLikePoisMix(y = as.matrix(y[v_star==0]), mean = list(exp(x %*% coef)[v_star==0], exp(X %*% coef)[v_star==0]), pi = lambda))
  }  # fixed the issue of numerical under-flow for log-likelihood of Poisson mixture

  # M step loglikelihood
  mstep.ll <- function(theta,z){
    -sum(                 dpois(x = y[v_star==1], lambda = exp(x %*% theta)[v_star==1], log = TRUE))-
      sum((1-z[v_star==0])*dpois(x = y[v_star==0], lambda = exp(X %*% theta)[v_star==0], log = TRUE)+
            z[v_star==0] *dpois(x = y[v_star==0], lambda = exp(x %*% theta)[v_star==0], log = TRUE))
  }

  old.obs.ll <- obs.ll(lambda, coef.reg)
  ll <- old.obs.ll

  # Number of digits (to the right of decimal point) printed to console will
  # depend on default user settings;
  num_digits <- getOption("digits")

  while(diff > epsilon && iter < maxit){

    # E-step
    # For solving the numerical under-flow issue for log-likelihood of Poisson mixture
    log.dens1 <- log(lambda[1]) + dpois(x = y, lambda = exp(xbeta), log = TRUE)
    log.dens1dens2 <- log.dens1

    for(i in 1:length(log.dens1dens2)){

      log.dens1dens2[i] <- as.numeric(mylogLikePoisMix(y = as.matrix(y[i]), pi = lambda, mean = list(exp(xbeta[i]), exp(X[i,] %*% theta) ) ))

    }

    z <- exp(log.dens1-log.dens1dens2)
    lambda.hat <- c(mean(z[v_star==0]), (1-mean(z[v_star==0])))

    #Non-linear minimization
    m <- try(suppressWarnings(nlm(f = mstep.ll, p = theta, z = z)), silent = TRUE)
    theta.hat <- m$estimate

    # Annoyingly, nlm() does not provide m$estimate as a named vector,
    # which consequently makes updating the xbeta object impossible.
    names(theta.hat) <- names(theta)

    new.obs.ll <- obs.ll(lambda.hat, theta.hat)
    diff <- new.obs.ll-old.obs.ll
    old.obs.ll <- new.obs.ll
    ll <- c(ll,old.obs.ll)
    lambda <- lambda.hat
    theta <- theta.hat

    if( length(theta[ -grep(v_star_name, names(theta)) ]) == 1 ){
      xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] * theta[ -grep(v_star_name, names(theta)) ] )
    }else{
      xbeta <- as.vector(x[, -grep(v_star_name, colnames(x)) ] %*% theta[ -grep(v_star_name, names(theta)) ] )
    }

    iter <- iter+1

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

  message("number of iterations = ", iter)


  # Make empty Hessian matrix;
  hess <- matrix(data = 0,  nrow = length(theta) + 1, ncol = length(theta) + 1,
                 dimnames = list( c("lambda", names(theta)), c("lambda", names(theta)) )  )

  # Element (1,1)
  hess[1,1] <- -sum( (1-v_star) * ( ( exp(0.5*(exp(x%*%theta) - exp(X%*%theta)) + 0.5*y*(X-x)%*%theta)  - exp( 0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*y*(x-X)%*%theta ) ) / ( lambda[2]*exp(0.5*(exp(x%*%theta) - exp(X%*%theta)) + 0.5*y*(X-x)%*%theta) + lambda[1] * exp( 0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*y*(x-X)%*%theta ) ) )^2 )

  # Main diagonal elements pertaining to regression coefficients;
  for(j in 1:ncol(x)){
    k <- j

    hess[k + 1, j + 1] <- sum(-v_star*x[,j]*x[,k]*exp(x%*%theta)
                              -(1-v_star)* ( lambda[2]^2 * X[,j]*X[,k] * ( (exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta)) + 0.5*X%*%theta))/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2
                                             + lambda[1]^2*x[,j]*x[,k] * ( (exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*x%*%theta) )/ (  lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)))  ) )^2
                                             -lambda[2]*lambda[1]*( ( x[,k]*(y-exp(x%*%theta))-X[,k]*(y-exp(X%*%theta)) )*( x[,j]*(y-exp(x%*%theta)) - X[,j]*(y-exp(X%*%theta)) ) - x[,j]*x[,k]*exp(x%*%theta) - X[,j]*X[,k]*exp(X%*%theta) ) * ( 1/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2  )  )
  }

  # Off diagonal elements pertaining to regression coefficients;
  for(i in 1:choose(ncol(x),2)){

    j <- combn(x = 1:ncol(x), m = 2)[1,i]
    k <- combn(x = 1:ncol(x), m = 2)[2,i]

    hess[k + 1, j + 1] <- sum(-v_star*x[,j]*x[,k]*exp(x%*%theta)
                              -(1-v_star)* ( lambda[2]^2 * X[,j]*X[,k] * ( (exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta)) + 0.5*X%*%theta))/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2
                                             + lambda[1]^2*x[,j]*x[,k] * ( (exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*x%*%theta) )/ (  lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)))  ) )^2
                                             -lambda[2]*lambda[1]*( ( x[,k]*(y-exp(x%*%theta))-X[,k]*(y-exp(X%*%theta)) )*( x[,j]*(y-exp(x%*%theta)) - X[,j]*(y-exp(X%*%theta)) ) - x[,j]*x[,k]*exp(x%*%theta) - X[,j]*X[,k]*exp(X%*%theta) ) * ( 1/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2  )  )

    hess[j + 1, k + 1] <- sum(-v_star*x[,j]*x[,k]*exp(x%*%theta)
                              -(1-v_star)* ( lambda[2]^2 * X[,j]*X[,k] * ( (exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta)) + 0.5*X%*%theta))/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2
                                             + lambda[1]^2*x[,j]*x[,k] * ( (exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*x%*%theta) )/ (  lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta)))  ) )^2
                                             -lambda[2]*lambda[1]*( ( x[,k]*(y-exp(x%*%theta))-X[,k]*(y-exp(X%*%theta)) )*( x[,j]*(y-exp(x%*%theta)) - X[,j]*(y-exp(X%*%theta)) ) - x[,j]*x[,k]*exp(x%*%theta) - X[,j]*X[,k]*exp(X%*%theta) ) * ( 1/( lambda[2]*exp(0.5*y*(X-x)%*%theta + 0.5*(exp(x%*%theta)-exp(X%*%theta))) + lambda[1]*exp(0.5*y*(x-X)%*%theta + 0.5*(exp(X%*%theta)-exp(x%*%theta))) ) )^2  )  )

  }

  # Covariances of lambda -- regression coefficients;
  for(j in 1:ncol(x)){
    hess[j + 1, 1] <- sum( (1-v_star) * (X[,j]*(y-exp(X%*%theta)) - x[,j]*(y-exp(x%*%theta))) * ( 1/( lambda[2]*exp(0.5*(exp(x%*%theta) - exp(X%*%theta)) + 0.5*y*(X-x)%*%theta) + lambda[1]*exp(0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*y*(x-X)%*%theta) ) )^2  )
    hess[1, j + 1] <- sum( (1-v_star) * (X[,j]*(y-exp(X%*%theta)) - x[,j]*(y-exp(x%*%theta))) * ( 1/( lambda[2]*exp(0.5*(exp(x%*%theta) - exp(X%*%theta)) + 0.5*y*(X-x)%*%theta) + lambda[1]*exp(0.5*(exp(X%*%theta)-exp(x%*%theta)) + 0.5*y*(x-X)%*%theta) ) )^2  )

  }


  # FIM is the negative of the  Hessian;
  FIM <- -hess

  # Then find std. errors;
  cov.pars.estimates <- solve(FIM)
  std.error <- sqrt(diag(cov.pars.estimates))

  # Calculate z values
  z_vals <- rep(NA, length(theta))
  z_vals <- theta / std.error[-1]


  # Calculate p-values of regression coefficients.
  p_vals <- rep(NA, length(z_vals))
  p_vals <- 2 * pnorm(q = abs(z_vals), lower.tail = F)

  # AIC, AICc, BIC
  # Note that theta does not contain lamdba1, hence the '+1' included.
  perf_metrics <- rep(NA, 3)
  AIC <- 2 * (length(theta) + 1 -  new.obs.ll)
  AICc <- AIC + (2 * (length(theta) + 1)^2 + 2 * (length(theta) + 1) )/(n - (length(theta) + 1) - 1)
  BIC <- log(n) * (length(theta) + 1) - 2 * new.obs.ll
  perf_metrics <- c(AIC, AICc, BIC)
  names(perf_metrics) <- c("AIC", "AICc", "BIC")


  # Output
  a <- list(y = y, lambda = lambda[2], params = theta, loglik = new.obs.ll,
            posterior = as.numeric(z), all.loglik = ll, cov.estimates = cov.pars.estimates,
            std.error = std.error, z.values = z_vals, p.values = p_vals,
            ICs = perf_metrics, ft = "poisRegMisrepEM", formula = formula, v_star_name = v_star_name)
  class(a) <- "misrepEM"
  a
}


