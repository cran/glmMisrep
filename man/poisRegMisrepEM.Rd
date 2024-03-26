\name{poisRegMisrepEM}
\alias{poisRegMisrepEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a Poisson Misrepresentation Model using EM Algorithm
}
\description{
\code{poisRegMisrepEM} is used to fit a Poisson regression model, adjusting for misrepresentation on a binary predictor. The function uses the Expectation Maximization algorithm and allows multiple additional correctly measured independent variables in the Poisson regression with a log-link function that is typically used in insurance claims modeling. Standard errors of model estimates are obtained from closed form expressions of the Observed Fisher Information.
}
\usage{
poisRegMisrepEM(formula, v_star, data, lambda = c(0.6,0.4),
                 epsilon = 1e-08, maxit = 10000,
                 maxrestarts = 20, verb = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.}

\item{v_star}{a character specifying the name of the binary predictor that is suspected of being misrepresented.}

\item{data}{a dataframe containing the variables in the model.}

\item{lambda}{initial mixing proportions used to start the EM algorithm. A numeric vector of length two, with the second element being the prevelance of misrepresentation.}

\item{epsilon}{tolerance for convergence. Convergence is reached when the log-likelihood increases by less than epsilon.}

\item{maxit}{the maximum number of iterations the EM routine will run for.}

\item{maxrestarts}{how many times the EM routine will attempt to converge. When conergence is not achieved, the EM routine restarts with new randomly selected mixing proportions.}

\item{verb}{logical. If TRUE, the difference in new .vs. old log-likelihood and the current log-likelihood is printed to the console after every iteration. If TRUE, the user will also be notifed if the EM algorithm must restart with new mixing proportions.}

}
\details{
Models for \code{poisRegMisrepEM} are specified symbolically. Like the \code{lm} and \code{glm} functions, the model has the form \code{response ~ terms}, where \code{response} is the numeric response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.

Currently, formula specification can accommodate the following expressions:

\itemize{
\item{transformations of the response: \code{log(y) ~ x}}
\item{polynomial terms: \code{y ~ x + I(x^2)}}
\item{interactions: \code{y ~ x*z}}
}

Including an offset term (e.g. \code{y ~ x + offset()}) is currently not supported.

}
\value{
\code{poisRegMisrepEM} returns an object of \code{\link{class}} \code{"misrepEM"}.

The function \code{summary} is used to obtain and print a summary of the results.

An object of class \code{"misrepEM"} is a list containing the following 14 elements:

\item{y}{the response used.}

\item{lambda}{numeric. The estimated prevelance of misrepresentation.}

\item{params}{a numeric vector containing the estimated parameters.}

\item{loglik}{the final maximized log-likelihood.}

\item{posterior}{a numeric vector. The posterior probability that the \emph{i-th} observation is not misrepresented for observations where the suspected misrepresented variable is zero, based on the last iteration of the EM algorithm. The values are not meaningful for observations where the suspected misrepresented variable is one.}

\item{all.loglik}{a numeric vector containing the log-likelihood at every iteration.}

\item{cov.estimates}{the inverse of the observed fisher information matrix evaluated at the maximum likelihood estimates.}

\item{std.error}{a numeric vector containing the standard errors of regression coefficients.}

\item{z.values}{a numeric vector containing the standardized regression coefficients.}

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

\donttest{
set.seed(314159)

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
a1 <- 0.5
a2 <- 0
a3 <- -1
a4 <- 2
a5 <- 1

mu <- rep(0, n)

for(i in 1:n){

  mu[i] <- exp(a0 + a1*X1 + a4*X3 + a5*V )[i]

  if(X2[i] == "a" || X2[i] == "b"){

    mu[i] <- mu[i]*exp(a2)

  }else{
    mu[i] <- mu[i]*exp(a3)
  }

}

Y <- rpois(n, mu)

data <- data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, V_star = V_star)

# "a" is the reference
data$X2 <- as.factor(data$X2)

# Model with main effects:
pois_mod <- poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                            v_star = "V_star", data = data)

# The prevalence of misrepresentation;
(theta0 * p0) / (1 - theta0*(1-p0)) # 0.09677419

# Parameter estimates and estimated prevalence of
# misrepresentation (lambda);
summary(pois_mod)

# Coefficients:
#             Estimate Std. Error   z value Pr(>|z|)
# (Intercept)  1.03519    0.02238  46.25615   <2e-16 ***
# X1           0.49875    0.01297  38.45157   <2e-16 ***
# X2b         -0.00007    0.01324  -0.00500  0.99601
# X2c         -0.98438    0.01926 -51.10084   <2e-16 ***
# X3           1.97794    0.00878 225.20267   <2e-16 ***
# V_star       0.99484    0.01290  77.14885   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 4170.836 4170.949 4205.190
# ---
# Log-Likelihood
#      -2078.418
# ---
# Lambda:  0.1039615 std.err:  0.01613403

# Fitting an interaction between X2 and X3;

a6 <- -0.5
a7 <- -0.5

for(i in 1:n){

  if(X2[i] == "c"){
    mu[i] <- mu[i]*exp(a6*X3[i])
  }else{
    if(X2[i] =="b"){
      mu[i] <- mu[i]*exp(a7*X3[i])
    }
  }
}

Y <- rpois(n, mu)

data$Y <- Y

pois_mod <- poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3,
                            v_star = "V_star", data = data)

summary(pois_mod)

# Coefficients:
#             Estimate Std. Error   z value Pr(>|z|)
# (Intercept)  0.98723    0.02917  33.84255   <2e-16 ***
# X1           0.50135    0.01540  32.56094   <2e-16 ***
# X2b         -0.03643    0.03655  -0.99648  0.31902
# X2c         -1.02315    0.05170 -19.79103   <2e-16 ***
# X3           1.99527    0.01319 151.22592   <2e-16 ***
# V_star       1.00917    0.01531  65.93335   <2e-16 ***
# X2b:X3      -0.47260    0.02137 -22.11569   <2e-16 ***
# X2c:X3      -0.49639    0.03018 -16.44530   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 4096.533 4096.714 4140.702
# ---
# Log-Likelihood
#      -2039.266
# ---
# Lambda:  0.1072814 std.err:  0.0162925

# Model fitting with a polynomial effect;

a8 <- -1

mu <- mu*exp(a8*X3^2)

Y <- rpois(n, mu)

data$Y <- Y

pois_mod <- poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3 + I(X3^2),
                            v_star = "V_star", data = data)

summary(pois_mod)

# Coefficients:
#             Estimate Std. Error   z value Pr(>|z|)
# (Intercept)  1.03291    0.04647  22.22701   <2e-16 ***
# X1           0.43783    0.03453  12.68058   <2e-16 ***
# X2b         -0.08042    0.05600  -1.43609  0.15098
# X2c         -1.02676    0.07523 -13.64912   <2e-16 ***
# X3           2.03183    0.06317  32.16597   <2e-16 ***
# V_star       0.98563    0.03415  28.86175   <2e-16 ***
# I(X3^2)     -0.99795    0.03529 -28.27715   <2e-16 ***
# X2b:X3      -0.45828    0.06499  -7.05189   <2e-16 ***
# X2c:X3      -0.47648    0.08912  -5.34623   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 3269.698 3269.920 3318.775
# ---
# Log-Likelihood
#      -1624.849
# ---
# Lambda:  0.108672 std.err:  0.02181499

}
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
