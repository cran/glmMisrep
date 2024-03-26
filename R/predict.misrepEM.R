predict.misrepEM <- function(object, newdata, ...){

  # First drop incomplete cases
  newdata <- newdata[ complete.cases(newdata), ]

  # Make design matrix
  x <- model.matrix(object = object$formula, data = newdata)

  # v_star variable
  v_star <- newdata[,object$v_star_name]

  # This other design matrix is made by first creating an alternative test
  # set where the v* variable is a constant 1.
  newdata_v <- newdata
  newdata_v[, object$v_star_name] <- 1

  X <- model.matrix(object = object$formula, data = newdata_v)

  # Estimated prevelance of misrepresentation;
  lambda <- object$lambda

  # If the model that was fit was a gamma, log-normal, or negative binomial,
  # then the first element of object$params is NOT a regression coefficient and
  # should be skipped/omitted.

  if( object$ft == "poisRegMisrepEM" ){

    preds <- v_star * exp( x %*% object$params ) + (1-v_star) * ( (1 - lambda)*exp( x %*% object$params ) + lambda*exp( X %*% object$params ) )

  }else{

    if(object$ft == "LnRegMisrepEM"){
      sigma <- object$params["sigma"]

      preds <- v_star * ( x %*% object$params[-1] - sigma^2/2 ) + (1-v_star) * ( (1 - lambda)*( x %*% object$params[-1] - sigma^2/2 ) + lambda*( X %*% object$params[-1] - sigma^2/2 ) )

    }else{

      if(object$ft == "NormRegMisrepEM"){

      preds <- v_star * ( x %*% object$params[-1] ) + (1-v_star) * ( (1 - lambda)*( x %*% object$params[-1] ) + lambda*( X %*% object$params[-1] ) )

      }else{

      preds <- v_star * exp( x %*% object$params[-1] ) + (1-v_star) * ( (1 - lambda)*exp( x %*% object$params[-1] ) + lambda*exp( X %*% object$params[-1] ) )

      }

    }
  }

  return(as.vector(preds))

}
