\name{NormRegMisrepEM}
\alias{NormRegMisrepEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a Linear Regression Misrepresentation Model using EM Algorithm
}
\description{
\code{NormRegMisrepEM} is used to fit a Linear regression model, adjusting for misrepresentation on a binary predictor. The function uses the Expectation Maximization algorithm and allows multiple additional correctly measured independent variables in the Normal regression with an identity link function that is typically used in insurance claims modeling. Standard errors of model estimates are obtained from closed form expressions of the Observed Fisher Information.
}
\usage{
NormRegMisrepEM(formula, v_star, data, lambda = c(0.6,0.4),
                 epsilon = 1e-08, maxit = 10000,
                 maxrestarts = 20, verb = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.}

\item{v_star}{a character specifying the name of the binary predictor that is suspected of being misrepresented.}

\item{data}{a dataframe containing the variables in the model.}

\item{lambda}{initial mixing proportions used to start the EM algorithm. A numeric vector of length two, with the second element being the prevalence of misrepresentation.}

\item{epsilon}{tolerance for convergence. Convergence is reached when the log-likelihood increases by less than epsilon.}

\item{maxit}{the maximum number of iterations the EM routine will run for.}

\item{maxrestarts}{how many times the EM routine will attempt to converge. When conergence is not achieved, the EM routine restarts with new randomly selected mixing proportions.}

\item{verb}{logical. If TRUE, the difference in new .vs. old log-likelihood and the current log-likelihood is printed to the console after every iteration. If TRUE, the user will also be notifed if the EM algorithm must restart with new mixing proportions.}

}
\details{
Models for \code{NormRegMisrepEM} are specified symbolically. Like the \code{lm} and \code{glm} functions, the model has the form \code{response ~ terms}, where \code{response} is the numeric response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.

Currently, formula specification can accommodate the following expressions:

\itemize{
\item{transformations of the response: \code{log(y) ~ x}}
\item{polynomial terms: \code{y ~ x + I(x^2)}}
\item{interactions: \code{y ~ x*z}}
}

Including an offset term (e.g. \code{y ~ x + offset()}) is currently not supported.
}
\value{
\code{NormRegMisrepEM} returns an object of \code{\link{class}} \code{"misrepEM"}.

The function \code{summary} is used to obtain and print a summary of the results.

An object of class \code{"misrepEM"} is a list containing the following 14 elements:

\item{y}{the response used.}

\item{lambda}{numeric. The estimated prevalence of misrepresentation.}

\item{params}{a numeric vector containing the estimated parameters.}

\item{loglik}{the final maximized log-likelihood.}

\item{posterior}{a numeric vector. The posterior probability that the \emph{i-th} observation is not misrepresented for observations where the suspected misrepresented variable is zero, based on the last iteration of the EM algorithm. The values are not meaningful for observations where the suspected misrepresented variable is one.}

\item{all.loglik}{a numeric vector containing the log-likelihood at every iteration.}

\item{cov.estimates}{the inverse of the observed fisher information matrix evaluated at the maximum likelihood estimates.}

\item{std.error}{a numeric vector containing the standard errors of regression coefficients.}

\item{t.values}{a numeric vector containing the standardized regression coefficients.}

\item{p.values}{a numeric vector containing the \emph{p-}values of the regression coefficients.}

\item{ICs}{a numeric vector of length three containing the AIC, AICc, and BIC.}

\item{ft}{a character containing the name of the function.}

\item{formula}{an object of class \code{formula} indicating the model that was fit.}

\item{v_star_name}{a character containing the name of the binary predictor suspected of misrepresentation.}
}
\references{
Xia, Michelle, Rexford Akakpo, and Matthew Albaugh. "Maximum Likelihood Approaches to Misrepresentation Models in GLM ratemaking: Model Comparisons." \emph{Variance} 16.1 (2023).

Akakpo, R. M., Xia, M., & Polansky, A. M. (2019). Frequentist inference in insurance ratemaking models adjusting for misrepresentation. \emph{ASTIN Bulletin: The Journal of the IAA, 49}(1), 117-146.

Xia, M., Hua, L., & Vadnais, G. (2018). Embedded predictive analysis of misrepresentation risk in GLM ratemaking models. \emph{Variance, 12}(1), 39-58.
}

\examples{

# Simulate data
n <- 1000
p0 <- 0.25

X1 <- rbinom(n, 1, 0.4)
X2 <- sample(x = c("a", "b", "c"), size = n, replace = TRUE)
X3 <- rnorm(n, 0, 1)

theta0 <- 0.3
V <- rbinom(n,1,theta0)
V_star <- V
V_star[V==1] <- rbinom(sum(V==1),1,1-p0)

a0 <- 1
a1 <- 2
a2 <- 0
a3 <- -1
a4 <- 4
a5 <- 2

mu <- rep(0, n)

for(i in 1:n){

  mu[i] <- (a0 + a1*X1 + a4*X3 + a5*V )[i]

  if(X2[i] == "a" || X2[i] == "b"){

    mu[i] <- mu[i] + a2

  }else{
    mu[i] <- mu[i] + a3
  }

}

sigma <- 0.427

Y <- rnorm(n, mu, sigma)

data <- data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, V_star = V_star)

# "a" is the reference
data$X2 <- as.factor(data$X2)

# Model with main effects:
norm_lm <- NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                              v_star = "V_star", data = data)

# The prevalence of misrepresentation;
(theta0 * p0) / (1 - theta0*(1-p0)) # 0.09677419

# Parameter estimates and estimated prevalence of
# misrepresentation (lambda);
summary(norm_lm)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  1.00624    0.02834  35.50820   <2e-16 ***
# X1           1.95903    0.02825  69.35263   <2e-16 ***
# X2b          0.04106    0.03413   1.20301  0.22926
# X2c         -1.00367    0.03418 -29.36328   <2e-16 ***
# X3           4.00031    0.01366 292.75308   <2e-16 ***
# V_star       2.01422    0.02922  68.93901   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 1674.683 1674.828 1713.945
# ---
# Log-Likelihood
#      -829.3415
# ---
# Lambda:  0.11085 std.err:  0.01150365

# Fitting an interaction between X2 and X3;

a6 <- -2
a7 <- 2

for(i in 1:n){

  if(X2[i] == "c"){
    mu[i] <- mu[i] + a6*X3[i]
  }else{
    if(X2[i] =="b"){
      mu[i] <- mu[i] + a7*X3[i]
    }
  }
}

Y <- rnorm(n, mu, sigma)

data$Y <- Y

norm_lm <- NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3,
                              v_star = "V_star", data = data)

summary(norm_lm)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.94905    0.02866  33.11281   <2e-16 ***
# X1           2.04258    0.02876  71.02223   <2e-16 ***
# X2b          0.00204    0.03463   0.05880  0.95313
# X2c         -0.97738    0.03469 -28.17313   <2e-16 ***
# X3           3.97014    0.02341 169.61108   <2e-16 ***
# V_star       2.01894    0.02967  68.04780   <2e-16 ***
# X2b:X3       2.00436    0.03459  57.95430   <2e-16 ***
# X2c:X3      -1.97573    0.03431 -57.59168   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 1668.925 1669.148 1718.003
# ---
# Log-Likelihood
#      -824.4626
# ---
# Lambda:  0.1055629 std.err:  0.01134299

# Model fitting with a polynomial effect;

a8 <- -0.5

mu <- mu + a8*X3^2

Y <- rnorm(n, mu, sigma)

data$Y <- Y

norm_lm <- NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3 + I(X3^2),
                              v_star = "V_star", data = data)

summary(norm_lm)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.95426    0.03050  31.28435   <2e-16 ***
# X1           2.00070    0.02878  69.52668   <2e-16 ***
# X2b          0.09309    0.03480   2.67463   0.0076  **
# X2c         -0.96572    0.03455 -27.95529   <2e-16 ***
# X3           3.96765    0.02378 166.82865   <2e-16 ***
# V_star       2.00513    0.02967  67.58481   <2e-16 ***
# I(X3^2)     -0.49043    0.00983 -49.90057   <2e-16 ***
# X2b:X3       2.04613    0.03454  59.24406   <2e-16 ***
# X2c:X3      -1.97248    0.03383 -58.30381   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 1672.933 1673.200 1726.918
# ---
# Log-Likelihood
#      -825.4665
# ---
# Lambda:  0.1061873 std.err:  0.01138759

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
