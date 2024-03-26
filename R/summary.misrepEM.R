summary.misrepEM <- function(object, ...){


  # First determine if a poisson regression was done or not.
  if(object$ft == "poisRegMisrepEM"){

    # If so, then the EM-mod object will have 'z.values'
    # as one of it's elements.
    names <- names(object$z.values)

    coefficients <- matrix(nrow = length(names), ncol = 4,
                           data = c( object$params[names], object$std.error[names],
                                     object$z.values[names], object$p.values[names]), byrow = F,
                           dimnames = list( c(names),
                                            c("Estimate", "Std. Error", "z value", "Pr(>|z|)")))
  }else{
    # Otherwise, the EM-mod object will have 't.values' instead;
    names <- names(object$t.values)

    coefficients <- matrix(nrow = length(names), ncol = 4,
                           data = c( object$params[names], object$std.error[names],
                                     object$t.values[names], object$p.values[names]), byrow = F,
                           dimnames = list( c(names),
                                            c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
  }

  sig_codes <- rep(NA, length(names))

  for(i in 1:nrow(coefficients)){
    if( 0 <= coefficients[i,4] & coefficients[i,4] < 0.001 ){
      sig_codes[i] <- "***"
    }else{
      if( 0.001 <= coefficients[i,4] & coefficients[i,4] < 0.01 ){
        sig_codes[i] <- "**"
      }else{
        if( 0.01 <= coefficients[i,4] & coefficients[i,4] < 0.05 ){
          sig_codes[i] <- "*"
        }else{
          if( 0.05 <= coefficients[i,4] & coefficients[i,4] < 0.1 ){
            sig_codes[i] <- "."
          }else{
            sig_codes[i] <- ""
          }
        }
      }
    }
  }

  coefficients[,1:4] <- apply(X = coefficients[,1:4], MARGIN = 2, FUN = round, digits = 5)

  coefficients <- as.data.frame(coefficients)

  coefficients[,4] <-  ifelse(coefficients[,4] < 2e-16, yes = "<2e-16",
                              no =  coefficients[,4])

  coefficients[,5] <- sig_codes

  names(coefficients)[5] <- ""

  coefficients

 # out_list <- list(coefficients,
 #                   object$ICs,
 #                   object$loglik,
 #                   object$lambda,
 #                   object$std.error["lambda"])

  # Give names to the elements of the list, so that we can flesh out the
  # \value{} section in summary.misrepEM.Rd
   out_list <- list(coefficients = coefficients,
                    ICs =  object$ICs,
                    loglik =  object$loglik,
                    lambda =  object$lambda,
                    lambda_stderror =  object$std.error["lambda"])

  class(out_list) <- "summary.misrepEM"

  out_list

}


print.summary.misrepEM <- function(x, ...){

  LL <-  x[[3]]

  names(LL) <- "Log-Likelihood"

  cat("Coefficients:", "\n")
  print(x[[1]])
  cat("---", "\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
  cat("---", "\n")
  print(x[[2]])
  cat("---", "\n")
  print(LL)
  cat("---", "\n")
  cat("Lambda: ", x[[4]], "std.err: ", x[[5]])

}

